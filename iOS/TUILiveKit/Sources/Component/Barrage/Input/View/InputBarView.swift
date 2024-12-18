//
//  InputBarView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/4.
//

import UIKit

let UIInputBarViewMinHeight: Int = 58
let UIInputTextViewMinHeight: Int = 42
let UIInputTextViewMaxHeight: Int = 90

protocol InputBarViewDelegate: AnyObject {
    func inputBarView(inputBarView: InputBarView, onSendText inputNormalText: String)
    func inputBarViewDidSwitchToEmotion(isEmotion: Bool)
}

class InputBarView: UIView {
    let buttonWidth: Int = 24
    let buttonHeight: Int = 24
    lazy var verticalPadding = (UIInputBarViewMinHeight - buttonHeight) / 2
    let horizontalPadding: Int = 6
    var textViewFrameX: CGFloat = 0
    var rightViewsMinX: CGFloat = 0
    let textViewHorizontalMargin: CGFloat = 8
    
    weak var delegate: InputBarViewDelegate?
    private var inputBarHeightChangeAnimationDuration: TimeInterval = 0.2
    private var inputBarHeightChangeAnimationWhenSendDuration: TimeInterval = 0.1
    private var keyboardSendEnabled = true
    private var previousTextViewContentHeight: CGFloat = 0
    private var clearInputTextBySendSoon: Bool = true

    lazy var emotionSwitchButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage(named: "live_emoji_icon", in: .liveBundle, compatibleWith: nil), for: .normal)
        button.setImage(UIImage(named: "live_barrage_softkeyboard", in: .liveBundle, compatibleWith: nil), for: .selected)
        button.addTarget(self, action: #selector(emotionSwitchButtonClick), for: .touchUpInside)
        return button
    }()

    lazy var rightSendButton: UIButton = {
        let button = UIButton()
        button.setTitle(.sendText, for: .normal)
        button.layer.cornerRadius = 18.scale375Height()
        button.backgroundColor = UIColor(red: 41 / 255.0, green: 204 / 255.0, blue: 106 / 255.0, alpha: 1)
        button.addTarget(self, action: #selector(rightSendButtonClick), for: .touchUpInside)
        return button
    }()

    lazy var inputTextView: UITextView = {
        let view = UITextView(frame: .zero)
        view.font = UIFont.systemFont(ofSize: 17.5)
        view.returnKeyType = UIReturnKeyType.send
        view.scrollsToTop = false
        view.textAlignment = .left
        view.textContainerInset = UIEdgeInsets(top: 10.0, left: 8.0, bottom: 10.0, right: 8.0)
        view.enablesReturnKeyAutomatically = true
        view.autoresizingMask = .flexibleWidth
        view.delegate = self
        view.showsVerticalScrollIndicator = false
        view.showsHorizontalScrollIndicator = false
        view.layer.cornerRadius = CGFloat(UIInputTextViewMinHeight / 2)
        view.textColor = UIColor(red: 213 / 255.0, green: 244 / 255.0, blue: 242 / 255.0, alpha: 0.6)
        view.backgroundColor = UIColor(red: 79 / 255.0, green: 88 / 255.0, blue: 107 / 255.0, alpha: 0.3)
        return view
    }()
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()        
        guard !isViewReady else { return }
        
        inputBarHeightChangeAnimationDuration = 0.2
        inputBarHeightChangeAnimationWhenSendDuration = 0.1

        constructViewHierarchy()
        activateConstraints()
        
        previousTextViewContentHeight = getTextViewContentHeight()
    }
    
    private func constructViewHierarchy() {
        addSubview(emotionSwitchButton)
        addSubview(inputTextView)
        addSubview(rightSendButton)
    }

    private func activateConstraints() {
        emotionSwitchButton.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(horizontalPadding)
            make.top.equalToSuperview().offset(verticalPadding)
            make.width.equalTo(buttonWidth)
            make.height.equalTo(buttonHeight)
        }
        rightSendButton.snp.makeConstraints { make in
            make.centerY.equalTo(emotionSwitchButton)
            make.trailing.equalToSuperview().offset(-horizontalPadding)
            make.size.equalTo(CGSize(width: 64.scale375Width(), height: 36.scale375Height()))
        }
        inputTextView.snp.makeConstraints { make in
            make.leading.equalTo(emotionSwitchButton.snp.trailing).offset(textViewHorizontalMargin)
            make.trailing.equalTo(rightSendButton.snp.leading).offset(-textViewHorizontalMargin)
            make.height.equalTo(UIInputTextViewMinHeight)
            make.top.equalToSuperview().offset((verticalPadding - 8).scale375Height())
            make.bottom.equalToSuperview().offset(-(verticalPadding - 8).scale375Height())
        }
    }
    
    @objc func emotionSwitchButtonClick(sender: UIButton) {
        sender.isSelected = !sender.isSelected
        delegate?.inputBarViewDidSwitchToEmotion(isEmotion: sender.isSelected)
    }

    @objc func rightSendButtonClick() {
        delegate?.inputBarView(inputBarView: self, onSendText: inputTextView.normalText)
        inputTextView.text = nil
    }
    
    @discardableResult
    override func becomeFirstResponder() -> Bool {
        super.becomeFirstResponder()
        return inputTextView.becomeFirstResponder()
    }
    
    @discardableResult
    override func resignFirstResponder() -> Bool {
        super.resignFirstResponder()
        return inputTextView.resignFirstResponder()
    }

    func insert(emotionImageAttributedString attributedString: NSAttributedString) {
        inputTextView.insert(emotionAttributedString: attributedString)
    }

    func insert(emotionKey: String) {
        inputTextView.insert(emotionKey: emotionKey)
    }

    func deleteEmotion() -> Bool {
        inputTextView.deleteEmotion()
    }

    func clearInputTextBySend() -> TimeInterval {
        let currentIsOneLine: Bool = Int(inputTextView.frame.size.height) == UIInputTextViewMinHeight
        inputTextView.text = nil
        clearInputTextBySendSoon = false
        return currentIsOneLine ? 0 : inputBarHeightChangeAnimationWhenSendDuration
    }

    private func getTextViewContentHeight() -> CGFloat {
        let size = inputTextView.sizeThatFits(inputTextView.frame.size)
        return (ceil(size.height) > CGFloat(UIInputTextViewMinHeight)) ? ceil(size.height) : CGFloat(UIInputTextViewMinHeight)
    }
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        backgroundColor = UIColor(red: 34 / 255.0, green: 38 / 255.0, blue: 46 / 255.0, alpha: 1)
        inputTextView.addObserver(self, forKeyPath: "contentSize", options: .new, context: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        inputTextView.removeObserver(self, forKeyPath: "contentSize")
    }
}

extension InputBarView: UITextViewDelegate {
    func textViewShouldBeginEditing(_ textView: UITextView) -> Bool {
        emotionSwitchButton.isSelected = false
        return true
    }

    func textViewDidBeginEditing(_ textView: UITextView) {
        if previousTextViewContentHeight != 0 {
            previousTextViewContentHeight = getTextViewContentHeight()
        }
    }

    func textView(_ textView: UITextView, shouldChangeTextIn range: NSRange, replacementText text: String) -> Bool {
        if keyboardSendEnabled && text == "\n" {
            delegate?.inputBarView(inputBarView: self, onSendText: textView.normalText)
            textView.text = nil
            return false
        }
        return true
    }
}

// MARK: KVO  This is only triggered when a line feed occurs

extension InputBarView {
    override func observeValue(forKeyPath keyPath: String?,
                               of object: Any?,
                               change: [NSKeyValueChangeKey: Any]?,
                               context: UnsafeMutableRawPointer?) {
        if inputTextView == object as? UITextView, keyPath == "contentSize" {
            let newContentHeight = getTextViewContentHeight()
            let heightShouldShrink = newContentHeight < previousTextViewContentHeight
            var textViewHeightShouldChangeValue = newContentHeight - previousTextViewContentHeight
            if !heightShouldShrink && (Int(previousTextViewContentHeight) == UIInputTextViewMaxHeight || inputTextView.text.isEmpty) {
                textViewHeightShouldChangeValue = 0
            } else {
                textViewHeightShouldChangeValue = min(textViewHeightShouldChangeValue,
                                                      CGFloat(UIInputTextViewMaxHeight) - previousTextViewContentHeight)
            }
            if textViewHeightShouldChangeValue != 0 {
                let animationDuration = clearInputTextBySendSoon ? inputBarHeightChangeAnimationWhenSendDuration : 
                inputBarHeightChangeAnimationDuration
                UIView.animate(withDuration: animationDuration) { [weak self] in
                    guard let self = self else { return }
                    inputTextView.snp.updateConstraints { [weak self] make in
                        guard let self = self else { return }
                        make.height.equalTo(CGRectGetHeight(inputTextView.frame) + textViewHeightShouldChangeValue)
                    }
                    inputTextView.layoutIfNeeded()
                }
                previousTextViewContentHeight = min(newContentHeight, CGFloat(UIInputTextViewMaxHeight))
            }

            clearInputTextBySendSoon = false
            if Int(previousTextViewContentHeight) == UIInputTextViewMaxHeight {
                let delayInSeconds = 0.01
                DispatchQueue.main.asyncAfter(deadline: .now() + delayInSeconds) { [weak self] in
                    guard let self = self else { return }
                    let bottomOffset = CGPoint(x: 0, y: newContentHeight - self.inputTextView.bounds.height)
                    inputTextView.setContentOffset(bottomOffset, animated: true)
                }
            }
        }
    }
}

private extension String {
    static let sendText = {
        localized("live.barrage.send")
    }()
}
