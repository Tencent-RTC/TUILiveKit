//
//  EmotionBoardView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/7.
//

import Foundation
import UIKit
import RTCCommon

protocol EmotionBoardViewDelegate: AnyObject {
    func emotionView(emotionBoardView: EmotionBoardView, didSelectEmotion emotion: Emotion, atIndex index: Int)
    func emotionViewDidSelectDeleteButton(emotionBoardView: EmotionBoardView)
}

protocol EmotionPageViewDelegate: AnyObject {
    func emotionPageView(emotionPageView: EmotionPageView, didSelectEmotion emotion: Emotion, atIndex index: Int)
}

class EmotionPageView: UIView {
    private let buttonWidth = 35
    private let buttonHeight = 30
    weak var delegate: EmotionPageViewDelegate?
    let emotionSelectedBackgroundView: UIView = {
        let view = UIView()
        view.isUserInteractionEnabled = false
        view.backgroundColor = UIColor(red: 0 / 255.0, green: 0 / 255.0, blue: 0 / 255.0, alpha: 0.16)
        view.layer.cornerRadius = 3
        view.alpha = 0
        return view
    }()

    var deleteButton: UIButton = UIButton()
    var deleteButtonOffset: CGPoint = CGPointZero
    var emotionLayers: [CALayer] = []
    var emotions: [Emotion] = []
    // Record the rect of the clickable area of all emoticons in the current pageView, updated in drawRect: and used in tap events
    var emotionHittingRects: [NSValue] = []
    var padding = UIEdgeInsets()
    var numberOfRows: Int = 4
    // The size of the drawing area for each expression
    var emotionSize = CGSize(width: 30, height: 30)
    var emotionSelectedBackgroundExtension = UIEdgeInsets()
    var minimumEmotionHorizontalSpacing: CGFloat = 16
    var needsLayoutEmotions: Bool = true
    var previousLayoutFrame = CGRect()

    override init(frame: CGRect) {
        super.init(frame: frame)
        backgroundColor = UIColor(red: 34 / 255.0, green: 38 / 255.0, blue: 46 / 255.0, alpha: 1)

        addSubview(emotionSelectedBackgroundView)
        let tap = UITapGestureRecognizer(target: self, action: #selector(handleTapGestureRecognizer))
        addGestureRecognizer(tap)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func frameForDeleteButton(deleteButton: UIView) -> CGRect {
        var rect = deleteButton.frame
        let x = CGRectGetWidth(bounds) - padding.right - CGRectGetWidth(deleteButton.frame) -
            (emotionSize.width - CGRectGetWidth(deleteButton.frame)) / 2.0 + deleteButtonOffset.x
        let y = CGRectGetHeight(bounds) - padding.bottom - CGRectGetHeight(deleteButton.frame) -
            (emotionSize.height - CGRectGetHeight(deleteButton.frame)) / 2.0 + deleteButtonOffset.y
        rect.origin = CGPoint(x: x, y: y)
        return rect
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        if deleteButton.superview == self {
            // The Delete button must be placed in the position of the last expression,
            // and is centered left and right above and below the expression
            deleteButton.frame = frameForDeleteButton(deleteButton: deleteButton)
        }
        let isSizeChanged = !CGSizeEqualToSize(previousLayoutFrame.size, frame.size)
        previousLayoutFrame = frame
        if isSizeChanged {
            setNeedsLayoutEmotions()
        }
        layoutEmotionsIfNeeded()
    }

    func setNeedsLayoutEmotions() {
        needsLayoutEmotions = true
    }

    func setEmotions(emotions: [Emotion]) {
        if self.emotions == emotions { return }
        self.emotions = emotions
        setNeedsLayoutEmotions()
        setNeedsLayout()
    }

    func layoutEmotionsIfNeeded() {
        if !needsLayoutEmotions { return }
        needsLayoutEmotions = false
        emotionHittingRects.removeAll()
        let contentSize = bounds.inset(by: padding).size
        let emotionCountPerRow = (contentSize.width + minimumEmotionHorizontalSpacing) / (emotionSize.width + minimumEmotionHorizontalSpacing)
        let emotionHorizontalSpacing = (contentSize.width - emotionCountPerRow * emotionSize.width) / (emotionCountPerRow - 1)
        let emotionVerticalSpacing = Int(contentSize.height - CGFloat(numberOfRows) * emotionSize.height) / Int(numberOfRows - 1)
        emotionSelectedBackgroundExtension = UIEdgeInsets(top: CGFloat(-emotionVerticalSpacing) / 2,
                                                          left: -emotionHorizontalSpacing / 2,
                                                          bottom: CGFloat(-emotionVerticalSpacing) / 2,
                                                          right: -emotionHorizontalSpacing / 2)
        var emotionOrigin = CGPointZero
        let emotionCount = emotions.count
        for i in stride(from: 0, to: emotionCount, by: 1) {
            var emotionLayer: CALayer
            if i < emotionLayers.count {
                emotionLayer = emotionLayers[i]
            } else {
                emotionLayer = CALayer()
                emotionLayer.contentsScale = UIScreen.main.scale
                emotionLayers.append(emotionLayer)
                layer.addSublayer(emotionLayer)
            }

            emotionLayer.contents = emotions[i].image.cgImage
            let row = i / Int(emotionCountPerRow)
            emotionOrigin.x = padding.left + (emotionSize.width + emotionHorizontalSpacing) * CGFloat(i % Int(emotionCountPerRow))
            emotionOrigin.y = padding.top + (emotionSize.height + CGFloat(emotionVerticalSpacing)) * CGFloat(row)
            let emotionRect = CGRect(x: emotionOrigin.x, y: emotionOrigin.y, width: emotionSize.width, height: emotionSize.height)
            let emotionHittingRect = emotionRect.inset(by: emotionSelectedBackgroundExtension)
            emotionHittingRects.append(NSValue(cgRect: emotionHittingRect))
            emotionLayer.frame = emotionRect
            emotionLayer.isHidden = false
        }
        if emotionLayers.count > emotionCount {
            for i in emotionLayers.count - emotionCount ..< emotionLayers.count {
                emotionLayers[i].isHidden = true
            }
        }
    }

    @objc func handleTapGestureRecognizer(_ gestureRecognizer: UITapGestureRecognizer) {
        let location = gestureRecognizer.location(in: self)
        for i in 0 ..< emotionHittingRects.count {
            let rect = emotionHittingRects[i].cgRectValue
            if rect.contains(location) {
                let layer = emotionLayers[i]
                if layer.opacity < 0.2 { return }
                let emotion = emotions[i]
                emotionSelectedBackgroundView.frame = rect
                UIView.animate(withDuration: 0.08, animations: { [weak self] in
                    guard let self = self else { return }
                    self.emotionSelectedBackgroundView.alpha = 1
                }, completion: { [weak self] _ in
                    guard let self = self else { return }
                    UIView.animate(withDuration: 0.08, animations: {
                        self.emotionSelectedBackgroundView.alpha = 0
                    }, completion: nil)
                })
                delegate?.emotionPageView(emotionPageView: self, didSelectEmotion: emotion, atIndex: i)
                return
            }
        }
    }

    func verticalSizeThatFits(size: CGSize, emotionVerticalSpacing: CGFloat) -> CGSize {
        let rect = CGRect(x: 0, y: 0, width: size.width, height: size.height)
        let contentSize = rect.inset(by: padding).size
        let emotionCountPerRow = (contentSize.width + minimumEmotionHorizontalSpacing) / (emotionSize.width + minimumEmotionHorizontalSpacing)
        let row = ceil(CGFloat(emotions.count) / (emotionCountPerRow * 1.0))
        let height = (emotionSize.height + emotionVerticalSpacing) * row - emotionVerticalSpacing + (padding.top + padding.bottom)
        return CGSize(width: size.width, height: height)
    }

    func updateDeleteButton(deleteButton: UIButton) {
        self.deleteButton = deleteButton
        addSubview(deleteButton)
    }

    func setDeleteButtonOffset(deleteButtonOffset: CGPoint) {
        self.deleteButtonOffset = deleteButtonOffset
        setNeedsLayout()
    }
}

class EmotionVerticalScrollView: UIScrollView {
    let pageView: EmotionPageView = {
        let pageView = EmotionPageView()
        pageView.deleteButton.isHidden = true
        return pageView
    }()

    override init(frame: CGRect) {
        super.init(frame: frame)
        addSubview(pageView)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func setEmotions(emotions: [Emotion],
                     emotionSize: CGSize,
                     minimumEmotionHorizontalSpacing: CGFloat,
                     emotionVerticalSpacing: CGFloat,
                     emotionSelectedBackgroundExtension: UIEdgeInsets,
                     paddingInPage: UIEdgeInsets) {
        let pageView = self.pageView
        pageView.emotions = emotions
        pageView.padding = paddingInPage
        let contentSize = CGSize(width: bounds.size.width - edgeInsetsGetHorizontalValue(insets: paddingInPage),
                                 height: bounds.size.height - edgeInsetsGetVerticalValue(insets: paddingInPage))
        let emotionCountPerRow = (contentSize.width + minimumEmotionHorizontalSpacing) / (emotionSize.width + minimumEmotionHorizontalSpacing)
        pageView.numberOfRows = Int(ceil(CGFloat(emotions.count) / emotionCountPerRow))
        pageView.emotionSize = emotionSize
        pageView.emotionSelectedBackgroundExtension = emotionSelectedBackgroundExtension
        pageView.minimumEmotionHorizontalSpacing = minimumEmotionHorizontalSpacing
        pageView.setNeedsLayout()
        let size = pageView.verticalSizeThatFits(size: bounds.size, emotionVerticalSpacing: emotionVerticalSpacing)
        self.pageView.frame = CGRect(x: 0, y: 0, width: size.width, height: size.height)
        self.contentSize = size
    }

    func edgeInsetsGetVerticalValue(insets: UIEdgeInsets) -> CGFloat {
        return insets.top + insets.bottom
    }

    func edgeInsetsGetHorizontalValue(insets: UIEdgeInsets) -> CGFloat {
        return insets.left + insets.right
    }
}

class EmotionBoardView: UIView {
    private let buttonWidth = 56.scale375()
    private let buttonHeight = 32.scale375Height()
    var emotions: [Emotion] = []
    weak var delegate: EmotionBoardViewDelegate?
    var deleteButtonMargins = UIEdgeInsets(top: 0, left: 0, bottom: 18, right: 18)
    var pagedEmotions: [Emotion] = []
    let emotionVerticalSpacing = CGFloat(14.5).scale375Height()
    let paddingInPage = UIEdgeInsets(top: 0, left: 16.scale375(), bottom: 0, right: 16.scale375())
    let numberOfRowsPerPage: Int = 4
    let emotionSize = CGSize(width: 32.scale375(), height: 32.scale375())
    let emotionSelectedBackgroundExtension = UIEdgeInsets(top: -3, left: -3, bottom: -3, right: 03)
    let minimumEmotionHorizontalSpacing: CGFloat = CGFloat(12.4).scale375Width()
    let deleteButtonOffset: CGPoint = CGPointZero
    let pageControlMarginBottom: CGFloat = 22

    lazy var verticalScrollView: EmotionVerticalScrollView = {
        let scrollView = EmotionVerticalScrollView()
        scrollView.contentInsetAdjustmentBehavior = .never
        return scrollView
    }()

    lazy var deleteButton: UIButton = {
        let button = UIButton()
        button.setImage(.barrageBundleImage("live_barrage_delete")?.withTintColor(.white.withAlphaComponent(0.9)), for: .normal)
        button.setImage(.barrageBundleImage("live_barrage_delete")?.withTintColor(.white.withAlphaComponent(0.14)), for: .disabled)
        button.addTarget(self, action: #selector(didSelectDeleteButton), for: .touchUpInside)
        button.layer.cornerRadius = 16.scale375Height()
        button.backgroundColor = .btnGrayColor
        return button
    }()

    lazy var topLineView: UIView = {
        let view = UIView()
        view.frame = CGRect(x: 0, y: 0, width: CGRectGetWidth(bounds), height: 1 / UIScreen.main.scale)
        view.backgroundColor = UIColor(red: 34 / 255.0, green: 38 / 255.0, blue: 46 / 255.0, alpha: 1)
        return view
    }()

    override init(frame: CGRect) {
        super.init(frame: frame)
        didInitialized(withFrame: frame)
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        didInitialized(withFrame: CGRectZero)
    }

    func didInitialized(withFrame frame: CGRect) {
        addSubview(verticalScrollView)
        addSubview(deleteButton)
        addSubview(topLineView)
    }

    func setEmotions(emotions: [Emotion]) {
        self.emotions = emotions
        setNeedsLayout()
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        deleteButton.frame = CGRect(x: bounds.width - deleteButtonMargins.right - buttonWidth,
                                    y: bounds.height - safeAreaInsets.bottom - deleteButtonMargins.bottom - buttonHeight,
                                    width: buttonWidth, height: buttonHeight)

        let verticalScrollViewFrame = bounds.inset(by: .zero)
        verticalScrollView.frame = verticalScrollViewFrame
        verticalScrollView.setEmotions(emotions: emotions,
                                       emotionSize: emotionSize,
                                       minimumEmotionHorizontalSpacing: minimumEmotionHorizontalSpacing,
                                       emotionVerticalSpacing: CGFloat(emotionVerticalSpacing),
                                       emotionSelectedBackgroundExtension: emotionSelectedBackgroundExtension,
                                       paddingInPage: paddingInPage)
        verticalScrollView.pageView.delegate = self
        topLineView.frame = CGRect(x: 0, y: 0, width: bounds.width, height: 1 / UIScreen.main.scale)
    }

    @objc func didSelectDeleteButton() {
        delegate?.emotionViewDidSelectDeleteButton(emotionBoardView: self)
    }
    
    func setDeleteBtnEnable(_ enable: Bool) {
        deleteButton.isEnabled = enable
    }
}

extension EmotionBoardView: EmotionPageViewDelegate {
    func emotionPageView(emotionPageView: EmotionPageView, didSelectEmotion emotion: Emotion, atIndex index: Int) {
        let index = emotions.firstIndex(of: emotion) ?? -1
        delegate?.emotionView(emotionBoardView: self, didSelectEmotion: emotion, atIndex: index)
    }
}
