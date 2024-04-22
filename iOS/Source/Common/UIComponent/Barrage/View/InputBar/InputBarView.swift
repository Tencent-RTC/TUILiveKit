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
    func inputBarView(inputBarView: InputBarView, inputTextViewShouldBeginEditing inputTextView: UITextView)
    func inputBarView(inputBarView: InputBarView, inputTextViewDidBeginEditing inputTextView: UITextView)
    func inputBarView(inputBarView: InputBarView, onKeyboardSendClick inputNormalText: String)
    func inputBarView(inputBarView: InputBarView, onEmotionButtonClick emotionSwitchButton: UIButton)
}

class InputBarView: UIView {
    let buttonWidth: Int = 24
    let buttonHeight: Int = 24
    lazy var verticalPadding = (UIInputBarViewMinHeight - buttonHeight) / 2
    let horizontalPadding: Int = 6
    var textViewFrameX: CGFloat = 0
    var rightViewsMinX: CGFloat = 0
    let textViewHorizontalMargin: CGFloat = 8
    let safeAreaInsetsLeft: CGFloat = {
        var value: CGFloat = 0.0
        if #available(iOS 15, *) {
            if let windowScene = UIApplication.shared.connectedScenes.first(where: { $0.activationState == .foregroundActive }) as? UIWindowScene {
                value = windowScene.windows.first?.safeAreaInsets.left ?? 0
            }
        } else {
            value = UIApplication.shared.windows.first?.safeAreaInsets.left ?? 0
        }
        return value
    }()
    let safeAreaInsetsRight: CGFloat = {
        var value: CGFloat = 0.0
        if #available(iOS 15, *) {
            if let windowScene = UIApplication.shared.connectedScenes.first(where: { $0.activationState == .foregroundActive }) as? UIWindowScene {
                value = windowScene.windows.first?.safeAreaInsets.right ?? 0
            }
        } else {
            value = UIApplication.shared.windows.first?.safeAreaInsets.right ?? 0
        }
        return value
    }()
    
    weak var delegate: InputBarViewDelegate?
    private var inputBarHeightChangeAnimationDuration: TimeInterval = 0.2
    private var inputBarHeightChangeAnimationWhenSendDuration: TimeInterval = 0.1
    private var keyboardSendEnabled = true
    private var previousTextViewContentHeight: CGFloat = 0
    private var clearInputTextBySendSoon: Bool = true

    lazy var emotionSwitchButton: UIButton = {
        let button = UIButton()
        button.frame = CGRect(x: horizontalPadding + Int(safeAreaInsetsLeft), y: verticalPadding, width: buttonWidth, height: buttonHeight)
        button.setBackgroundImage(UIImage(named: "live_emoji_icon", in: Bundle.liveBundle, compatibleWith: nil), for: .normal)
        button.setBackgroundImage(UIImage(named: "live_barrage_softKeyboard", in: Bundle.liveBundle, compatibleWith: nil), for: .selected)
        button.addTarget(self, action: #selector(emotionSwitchButtonClick), for: .touchUpInside)
        return button
    }()

    lazy var rightSendButton: UIButton = {
        let button = UIButton()
        let offsetX: CGFloat = self.bounds.width - CGFloat(horizontalPadding) - 64 - CGFloat(safeAreaInsetsRight)
        let offsetY: CGFloat = CGFloat(verticalPadding - 4)
        button.frame = CGRect(x: offsetX, y: offsetY, width: 64, height: 36)
        button.setTitle(.sendText, for: .normal)
        button.layer.cornerRadius = 18
        button.addTarget(self, action: #selector(rightSendButtonClick), for: .touchUpInside)
        return button
    }()

    lazy var inputTextView: UITextView = {
        textViewFrameX = CGRectGetMaxX(emotionSwitchButton.frame)
        rightViewsMinX = CGRectGetMinX(rightSendButton.frame)
        let textViewWidth = rightViewsMinX - textViewHorizontalMargin - textViewFrameX - textViewHorizontalMargin

        let view = UITextView(frame: .zero)
        view.font = UIFont.systemFont(ofSize: 17.5)
        view.returnKeyType = UIReturnKeyType.send
        view.scrollsToTop = false
        view.textAlignment = .left
        view.layer.cornerRadius = 6.0
        view.textContainerInset = UIEdgeInsets(top: 10.0, left: 8.0, bottom: 10.0, right: 8.0)
        view.enablesReturnKeyAutomatically = true
        view.autoresizingMask = .flexibleWidth
        view.delegate = self
        view.showsVerticalScrollIndicator = false
        view.showsHorizontalScrollIndicator = false
        view.layer.cornerRadius = CGFloat(UIInputTextViewMinHeight / 2)
        view.frame = CGRect(x: textViewFrameX + textViewHorizontalMargin, y: CGFloat(verticalPadding - 8),
                            width: textViewWidth, height: CGFloat(UIInputTextViewMinHeight))
        return view
    }()

    lazy var topLineView: UIView = {
        let view = UIView(frame: CGRect(x: 0, y: 0, width: Int(CGRectGetWidth(self.bounds)), height: 1 / Int(UIScreen.main.scale)))
        return view
    }()

    // this function only needs to be called once
    func setup() {
        previousTextViewContentHeight = getTextViewContentHeight()

        backgroundColor = UIColor(red: 34 / 255.0, green: 38 / 255.0, blue: 46 / 255.0, alpha: 1)
        inputTextView.textColor = UIColor(red: 213 / 255.0, green: 244 / 255.0, blue: 242 / 255.0, alpha: 0.6)
        inputTextView.backgroundColor = UIColor(red: 79 / 255.0, green: 88 / 255.0, blue: 107 / 255.0, alpha: 0.3)
        rightSendButton.backgroundColor = UIColor(red: 41 / 255.0, green: 204 / 255.0, blue: 106 / 255.0, alpha: 1)
        topLineView.backgroundColor = UIColor(red: 41 / 255.0, green: 204 / 255.0, blue: 106 / 255.0, alpha: 1)

        inputBarHeightChangeAnimationDuration = 0.2
        inputBarHeightChangeAnimationWhenSendDuration = 0.1

        addSubview(emotionSwitchButton)
        addSubview(inputTextView)
        addSubview(rightSendButton)
        addSubview(topLineView)

        inputTextView.addObserver(self, forKeyPath: "contentSize", options: .new, context: nil)
    }

    @objc func emotionSwitchButtonClick(sender: UIButton) {
        sender.isSelected = !sender.isSelected
        delegate?.inputBarView(inputBarView: self, onEmotionButtonClick: sender)
    }

    @objc func rightSendButtonClick() {
        delegate?.inputBarView(inputBarView: self, onKeyboardSendClick: inputTextView.normalText)
    }

    func resetTextViewHeightBy(textViewHeightShouldChangeValue height: CGFloat) {
        let prevFrame = inputTextView.frame
        inputTextView.frame = CGRect(x: prevFrame.origin.x,
                                     y: prevFrame.origin.y,
                                     width: prevFrame.size.width,
                                     height: prevFrame.size.height + height)
    }

    func textViewBecomeFirstResponder() {
        inputTextView.becomeFirstResponder()
    }

    func textViewResignFirstResponder() {
        inputTextView.resignFirstResponder()
    }

    func textViewInputNormalText() -> String {
        return inputTextView.normalText
    }

    func insertEmotionAttributedString(emotionImageAttributedString attributedString: NSAttributedString) {
        inputTextView.insertEmotionAttributedString(emotionAttributedString: attributedString)
    }

    func insertEmotion(emotionKey: String) {
        inputTextView.insertEmotionKey(emotionKey: emotionKey)
    }

    func deleteEmotion() -> Bool {
        inputTextView.deleteEmotion()
    }

    func clearInputTextBySend() -> TimeInterval {
        let currentIsOneLine: Bool = Int(inputTextView.frame.size.height) == UIInputTextViewMinHeight
        clearInputTextBySendSoon = true
        inputTextView.text = nil
        clearInputTextBySendSoon = false
        return currentIsOneLine ? 0 : inputBarHeightChangeAnimationWhenSendDuration
    }

    private func getTextViewContentHeight() -> CGFloat {
        let size = inputTextView.sizeThatFits(inputTextView.frame.size)
        return (ceil(size.height) > CGFloat(UIInputTextViewMinHeight)) ? ceil(size.height) : CGFloat(UIInputTextViewMinHeight)
    }

    deinit {
        inputTextView.removeObserver(self, forKeyPath: "contentSize")
        inputTextView.delegate = nil
    }
}

extension InputBarView: UITextViewDelegate {
    func textViewShouldBeginEditing(_ textView: UITextView) -> Bool {
        delegate?.inputBarView(inputBarView: self, inputTextViewShouldBeginEditing: inputTextView)
        emotionSwitchButton.isSelected = false
        return true
    }

    func textViewDidBeginEditing(_ textView: UITextView) {
        inputTextView.becomeFirstResponder()
        if previousTextViewContentHeight != 0 {
            previousTextViewContentHeight = getTextViewContentHeight()
        }
        delegate?.inputBarView(inputBarView: self, inputTextViewDidBeginEditing: inputTextView)
    }

    func textViewDidEndEditing(_ textView: UITextView) {
        inputTextView.resignFirstResponder()
    }

    func textView(_ textView: UITextView, shouldChangeTextIn range: NSRange, replacementText text: String) -> Bool {
        if keyboardSendEnabled && text == "\n" {
            delegate?.inputBarView(inputBarView: self, onKeyboardSendClick: textView.normalText)
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
                    if heightShouldShrink {
                        self.resetTextViewHeightBy(textViewHeightShouldChangeValue: textViewHeightShouldChangeValue)
                    }

                    var inputBarViewFrame = self.frame
                    inputBarViewFrame.origin.y -= textViewHeightShouldChangeValue
                    inputBarViewFrame.size.height += textViewHeightShouldChangeValue
                    self.frame = inputBarViewFrame

                    if !heightShouldShrink {
                        // In order to be compatible with earlier versions of iOS, all this code cannot be combined with the above
                        self.resetTextViewHeightBy(textViewHeightShouldChangeValue: textViewHeightShouldChangeValue)
                    }
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
