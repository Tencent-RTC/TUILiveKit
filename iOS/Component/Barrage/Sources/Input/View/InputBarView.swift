//
//  InputBarView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/4.
//

import UIKit

let UIInputBarViewMinHeight = 58.scale375Height()
let UIInputTextViewMinHeight = 32.scale375Height()
let UIInputTextViewMaxHeight = 90.scale375Height()

protocol InputBarViewDelegate: AnyObject {
    func inputBarView(inputBarView: InputBarView, onSendText inputNormalText: String)
    func inputBarViewDidSwitchToEmotion(isEmotion: Bool)
    func inputBarEmptyChanged(isEmpty: Bool)
}

class InputBarView: UIView {
    let buttonWidth = 28.scale375()
    let buttonHeight = 28.scale375()
    lazy var verticalPadding = (UIInputBarViewMinHeight - buttonHeight) / 2
    let horizontalPadding = 12.scale375()
    var textViewFrameX: CGFloat = 0
    var rightViewsMinX: CGFloat = 0
    let textViewHorizontalMargin: CGFloat = 8.scale375()
    
    weak var delegate: InputBarViewDelegate?
    private var inputBarHeightChangeAnimationDuration: TimeInterval = 0.2
    private var inputBarHeightChangeAnimationWhenSendDuration: TimeInterval = 0.1
    private var keyboardSendEnabled = true
    private var previousTextViewContentHeight: CGFloat = 0
    private var clearInputTextBySendSoon: Bool = true

    lazy var emotionSwitchButton: UIButton = {
        let button = UIButton()
        button.setImage(.barrageBundleImage("live_emoji_icon"), for: .normal)
        button.setImage(.barrageBundleImage("live_barrage_softkeyboard"), for: .selected)
        button.addTarget(self, action: #selector(emotionSwitchButtonClick), for: .touchUpInside)
        return button
    }()

    lazy var rightSendButton: UIButton = {
        let button = UIButton()
        button.setTitle(.sendText, for: .normal)
        button.setTitleColor(.white, for: .normal)
        button.setTitleColor(.white.withAlphaComponent(0.42), for: .disabled)
        button.titleLabel?.font = .customFont(ofSize: 15, weight: .medium)
        button.layer.cornerRadius = 18.scale375Height()
        button.backgroundColor = .b1
        button.addTarget(self, action: #selector(rightSendButtonClick), for: .touchUpInside)
        return button
    }()

    lazy var inputTextView: UITextView = {
        let view = UITextView(frame: .zero)
        view.font = UIFont.customFont(ofSize: 15, weight: .medium)
        view.returnKeyType = UIReturnKeyType.send
        view.scrollsToTop = false
        view.textAlignment = .left
        view.textContainerInset = UIEdgeInsets(top: 6.scale375Height(), left: 8.0, bottom: 6.scale375Height(), right: 8.0)
        view.enablesReturnKeyAutomatically = true
        view.autoresizingMask = .flexibleWidth
        view.delegate = self
        view.showsVerticalScrollIndicator = false
        view.showsHorizontalScrollIndicator = false
        view.layer.cornerRadius = CGFloat(UIInputTextViewMinHeight / 2)
        view.textColor = .white.withAlphaComponent(0.9)
        view.backgroundColor = UIColor(red: 79 / 255.0, green: 88 / 255.0, blue: 107 / 255.0, alpha: 0.3)
        view.text = BarrageManager.shared.inputString
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
        initSendButtonStatus()
    }
    
    private func constructViewHierarchy() {
        addSubview(emotionSwitchButton)
        addSubview(inputTextView)
        addSubview(rightSendButton)
    }

    private func activateConstraints() {
        emotionSwitchButton.snp.makeConstraints { make in
            make.leading.equalTo(inputTextView.snp.trailing).offset(textViewHorizontalMargin)
            make.centerY.equalTo(inputTextView)
            make.width.equalTo(buttonWidth)
            make.height.equalTo(buttonHeight)
        }
        rightSendButton.snp.makeConstraints { make in
            make.centerY.equalTo(emotionSwitchButton)
            make.leading.equalTo(emotionSwitchButton.snp.trailing).offset(textViewHorizontalMargin)
            make.trailing.equalToSuperview().offset(-horizontalPadding)
            make.size.equalTo(CGSize(width: 64.scale375Width(), height: 32.scale375Height()))
        }
        inputTextView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(horizontalPadding)
            make.trailing.equalTo(emotionSwitchButton.snp.leading).offset(-textViewHorizontalMargin)
            make.height.equalTo(UIInputTextViewMinHeight)
            make.top.equalToSuperview().offset(verticalPadding - 4.scale375Height())
            make.bottom.equalToSuperview().offset(-(verticalPadding - 4.scale375Height()))
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
        let currentIsOneLine: Bool = inputTextView.frame.size.height == UIInputTextViewMinHeight
        inputTextView.text = nil
        clearInputTextBySendSoon = false
        return currentIsOneLine ? 0 : inputBarHeightChangeAnimationWhenSendDuration
    }

    private func getTextViewContentHeight() -> CGFloat {
        let size = inputTextView.sizeThatFits(inputTextView.frame.size)
        return (ceil(size.height) > CGFloat(UIInputTextViewMinHeight)) ? ceil(size.height) : CGFloat(UIInputTextViewMinHeight)
    }
    
    private func initSendButtonStatus() {
        let isEnabled = !BarrageManager.shared.inputString.isEmpty
        rightSendButton.isEnabled = isEnabled
        rightSendButton.backgroundColor = isEnabled ? .b1 : .btnDisabledColor
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
    
    func textViewDidEndEditing(_ textView: UITextView) {
        BarrageManager.shared.inputString = textView.text
    }
    
    func textView(_ textView: UITextView, shouldChangeTextIn range: NSRange, replacementText text: String) -> Bool {
        if keyboardSendEnabled && text == "\n" {
            delegate?.inputBarView(inputBarView: self, onSendText: textView.normalText)
            textView.text = nil
            return false
        }
        return true
    }
    
    func textViewDidChange(_ textView: UITextView) {
        if textView.text.isEmpty {
            rightSendButton.isEnabled = false
            rightSendButton.backgroundColor = .btnDisabledColor
        } else {
            rightSendButton.isEnabled = true
            rightSendButton.backgroundColor = .b1
        }
        delegate?.inputBarEmptyChanged(isEmpty: textView.text.isEmpty)
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
            if !heightShouldShrink && (previousTextViewContentHeight == UIInputTextViewMaxHeight || inputTextView.text.isEmpty) {
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
            if previousTextViewContentHeight == UIInputTextViewMaxHeight {
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
        localized("Send")
    }()
}
