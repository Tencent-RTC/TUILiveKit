//
//  InputBoardManager.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/8.
//

import UIKit

enum InputState {
    case none
    case text
    case emotion
}

protocol InputBoardManagerDelegate: AnyObject {
    func inputBoardManager(inputBoardManager: InputBoardManager,
                           onClickSendButton inputText: String)
}

class InputBoardManager {
    private weak var viewController: UIViewController?
    private var inputBarBelowViewController: Bool = false
    private var currentInputState: InputState = .none
    private let safeAreaInsetsBottom: CGFloat = {
        var value: CGFloat = 0.0
        if #available(iOS 15, *) {
            if let windowScene = UIApplication.shared.connectedScenes.first(where: { $0.activationState == .foregroundActive }) as? UIWindowScene {
                value = windowScene.windows.first?.safeAreaInsets.bottom ?? 0
            }
        } else {
            value = UIApplication.shared.windows.first?.safeAreaInsets.bottom ?? 0
        }
        return value
    }()
    
    private var viewControllerWillDisappear: Bool = false
    weak var delegate: InputBoardManagerDelegate?

    private lazy var inputBarView: InputBarView = {
        let inputBarView = InputBarView()
        inputBarView.frame = CGRect(x: 0, y: 100, width: UIScreen.main.bounds.width, height: 58)
        inputBarView.delegate = self
        inputBarView.setup()
        return inputBarView
    }()

    private lazy var belowInputBarView: UIView = {
        guard let viewController = viewController else { return UIView()}
        let belowInputBarViewHeight = 275
        let view = UIView(frame: CGRect(x: 0, y: Int(viewController.view.frame.size.height),
                                        width: Int(viewController.view.frame.size.width), height: belowInputBarViewHeight))
        view.autoresizingMask = [.flexibleWidth, .flexibleTopMargin]
        view.backgroundColor = inputBarView.backgroundColor
        return view
    }()

    private lazy var emotionBoardView: EmotionBoardView = {
        let emotionBoardView = EmotionBoardView()
        let emotionHelper = EmotionHelper.shared
        emotionHelper.useDefaultEmotions()
        emotionBoardView.emotions = emotionHelper.emotions
        emotionBoardView.delegate = self
        emotionBoardView.backgroundColor = UIColor(red: 34 / 255.0, green: 38 / 255.0, blue: 46 / 255.0, alpha: 1)
        let emotionBoardHeight = 274.0
        guard let viewController = viewController else { return emotionBoardView }
        emotionBoardView.frame = CGRect(x: 0, y: CGRectGetHeight(viewController.view.bounds),
                                        width: CGRectGetWidth(viewController.view.bounds),
                                        height: emotionBoardHeight)
        emotionBoardView.alpha = 0

        return emotionBoardView
    }()

    init(withViewController viewController: UIViewController) {
        self.viewController = viewController
        addObserver()
        viewController.view.addSubview(belowInputBarView)
        viewController.view.bringSubviewToFront(belowInputBarView)
        viewController.view.addSubview(emotionBoardView)
        viewController.view.bringSubviewToFront(emotionBoardView)
        viewController.view.addSubview(inputBarView)
        viewController.view.bringSubviewToFront(inputBarView)
        updateInputBarViewFrame()
    }

    deinit {
        removeObserver()
    }

    func updateInputBarViewFrame() {
        guard let viewController = viewController else { return }
        inputBarView.autoresizingMask = [.flexibleWidth, .flexibleTopMargin]
        let inputFrameY = Int(viewController.view.frame.size.height - inputBarView.frame.size.height - safeAreaInsetsBottom)
        let inputFrame: CGRect = CGRect(x: 0, y: inputFrameY,
                                        width: Int(viewController.view.frame.size.width),
                                        height: Int(inputBarView.frame.size.height))
        inputBarView.frame = inputFrame
        inputBarBelowViewController = false
    }

    func showInputBarView() {
        inputBarView.isHidden = false
        belowInputBarView.isHidden = false
        inputBarView.textViewBecomeFirstResponder()
    }

    func hideInputBarView() {
        inputBarView.isHidden = true
        belowInputBarView.isHidden = true
        inputBarView.textViewResignFirstResponder()
    }

    func hideBoardView() {
        if currentInputState != .none {
            hideKeyboardAndSwitchBoardView(hideBoard: true)
        }
    }

    func switchToEmotionMode() {
        currentInputState = .emotion
        hideKeyboardAndSwitchBoardView(hideBoard: false)
    }

    private func hideKeyboardAndSwitchBoardView(hideBoard: Bool) {
        if hideBoard && currentInputState == .text {
            currentInputState = .none
        }
        
        inputBarView.textViewResignFirstResponder()
        UIView.animate(withDuration: 0.2, delay: 0, options: UIView.AnimationOptions.curveEaseInOut) { [weak self] in
            guard let self = self else { return }
            var currentBoardViewFrame: CGRect = CGRectZero

            switch self.currentInputState {
            case .emotion:
                currentBoardViewFrame = setEmotionBoardVisibility(show: !hideBoard)
            default:
                break
            }

            self.layoutInputBarView(currentBoardViewMinY: CGRectGetMinY(currentBoardViewFrame), hideBoard: hideBoard)
        } completion: { [weak self] _ in
            guard let self = self else { return }
            if hideBoard {
                self.currentInputState = .none
            }
        }
    }

    private func setEmotionBoardVisibility(show: Bool) -> CGRect {
        var prevFrame = emotionBoardView.frame
        guard let viewController = viewController else { return prevFrame }
        prevFrame.origin.y = !show ? CGRectGetHeight(viewController.view.frame) :
            (CGRectGetHeight(viewController.view.frame) - CGRectGetHeight(prevFrame))
        emotionBoardView.alpha = show ? 1 : 0
        emotionBoardView.frame = prevFrame
        return prevFrame
    }

    private func layoutInputBarView(currentBoardViewMinY: CGFloat, hideBoard: Bool) {
        var prevInputViewFrame = inputBarView.frame
        guard let viewController = viewController else { return }
        if hideBoard {
            var hidedFrameY = CGRectGetHeight(viewController.view.bounds)
            if !inputBarBelowViewController {
                hidedFrameY -= CGRectGetHeight(prevInputViewFrame)
                hidedFrameY -= safeAreaInsetsBottom
            }
            prevInputViewFrame.origin.y = hidedFrameY
        } else {
            prevInputViewFrame.origin.y = currentBoardViewMinY - CGRectGetHeight(prevInputViewFrame)
        }
        inputBarView.frame = prevInputViewFrame
        let frame = belowInputBarView.frame
        belowInputBarView.frame = CGRect(x: frame.origin.x, y: CGRectGetMaxY(inputBarView.frame), width: frame.size.width, height: frame.size.height)
    }
}

// MARK: keyboard listener

extension InputBoardManager {
    func addObserver() {
        NotificationCenter.default.addObserver(self, 
                                               selector: #selector(onKeyboardWillShow),
                                               name: UIResponder.keyboardWillShowNotification, object: nil)
        NotificationCenter.default.addObserver(self,
                                               selector: #selector(onKeyboardWillHide),
                                               name: UIResponder.keyboardWillHideNotification, object: nil)
    }
    

    func removeObserver() {
        NotificationCenter.default.removeObserver(self)
    }

    @objc func onKeyboardWillShow(notification: Notification) {
        guard let keyboardRect: CGRect = notification.userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? CGRect,
              let curveRawValue: Int = notification.userInfo?[UIResponder.keyboardAnimationCurveUserInfoKey] as? Int,
              let duration = notification.userInfo?[UIResponder.keyboardAnimationDurationUserInfoKey] as? Double,
              let curve = UIView.AnimationCurve(rawValue: curveRawValue)
        else { return }
        onKeyboardWillShowOrHide(keyboardRect: keyboardRect,
                                 animationOptions: animationOptionsForCurve(curve: curve),
                                 duration: duration,
                                 showKeyboard: true)
    }

    @objc func onKeyboardWillHide(notification: Notification) {
        guard let keyboardRect: CGRect = notification.userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? CGRect,
              let curveRawValue: Int = notification.userInfo?[UIResponder.keyboardAnimationCurveUserInfoKey] as? Int,
              let duration = notification.userInfo?[UIResponder.keyboardAnimationDurationUserInfoKey] as? Double,
              let curve = UIView.AnimationCurve(rawValue: curveRawValue)
        else { return }
        onKeyboardWillShowOrHide(keyboardRect: keyboardRect,
                                 animationOptions: animationOptionsForCurve(curve: curve),
                                 duration: duration,
                                 showKeyboard: false)
    }

    private func onKeyboardWillShowOrHide(keyboardRect: CGRect, animationOptions: UIView.AnimationOptions,
                                          duration: Double, showKeyboard: Bool) {
        if viewControllerWillDisappear {
            // In order to solve the bug that the vc side slides back when
            // the keyboard is eject, the Notification is triggered, which affects inputBarView.frame
            return
        }

        let previousInputState: InputState = currentInputState

        if showKeyboard {
            currentInputState = .text
        }

        if currentInputState == .text {
            guard let viewController = viewController else { return }
            let keyboardY = viewController.view.convert(keyboardRect, from: nil).origin.y
            layoutInputBarView(currentBoardViewMinY: keyboardY, hideBoard: !showKeyboard)
            if showKeyboard {
                UIView.animate(withDuration: duration, delay: 0, options: animationOptions) { [weak self] in
                    guard let self = self else { return }
                    switch previousInputState {
                    case .emotion:
                        _ = self.setEmotionBoardVisibility(show: false)
                    default:
                        break
                    }
                }
            } else {
                currentInputState = .none
                hideInputBarView()
                viewController.dismiss(animated: true)
            }
        }
    }

    func animationOptionsForCurve(curve: UIView.AnimationCurve) -> UIView.AnimationOptions {
        switch curve {
        case .easeInOut:
            return UIView.AnimationOptions.curveEaseInOut
        case .easeIn:
            return UIView.AnimationOptions.curveEaseIn
        case .easeOut:
            return UIView.AnimationOptions.curveEaseOut
        case .linear:
            return UIView.AnimationOptions.curveLinear
        default:
            return UIView.AnimationOptions.curveEaseInOut
        }
    }
}

extension InputBoardManager: InputBarViewDelegate {
    func inputBarView(inputBarView: InputBarView, inputTextViewShouldBeginEditing inputTextView: UITextView) {
    }

    func inputBarView(inputBarView: InputBarView, inputTextViewDidBeginEditing inputTextView: UITextView) {
    }

    func inputBarView(inputBarView: InputBarView, onKeyboardSendClick inputNormalText: String) {
        delegate?.inputBoardManager(inputBoardManager: self, onClickSendButton: inputNormalText)
        inputBarView.inputTextView.text = nil
    }

    func inputBarView(inputBarView: InputBarView, onEmotionButtonClick emotionSwitchButton: UIButton) {
        if emotionSwitchButton.isSelected {
            switchToEmotionMode()
        } else {
            inputBarView.textViewBecomeFirstResponder()
        }
    }
}

extension InputBoardManager: EmotionBoardViewDelegate {
    func emotionView(emotionBoardView: EmotionBoardView, didSelectEmotion emotion: Emotion, atIndex index: Int) {
        let emotionHelper = EmotionHelper.shared
        let attributedString = emotionHelper.obtainImageAttributedString(byImageKey: emotion.displayName,
                                                                         font: inputBarView.inputTextView.font ?? UIFont(), useCache: false)
        inputBarView.insertEmotionAttributedString(emotionImageAttributedString: attributedString)
    }

    func emotionViewDidSelectDeleteButton(emotionBoardView: EmotionBoardView) {
        if !inputBarView.deleteEmotion() {
            inputBarView.inputTextView.deleteBackward()
        }
    }
}
