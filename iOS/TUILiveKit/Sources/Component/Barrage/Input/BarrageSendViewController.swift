//
//  BarrageSendViewController.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/15.
//

import UIKit
import Combine
import RTCCommon
import RTCRoomEngine
import AtomicXCore

class BarrageSendViewController: UIViewController {
    
    enum InputState {
        case none
        case text
        case emotion
    }
    
    private let roomId: String
    private var cancellableSet = Set<AnyCancellable>()
        
    private lazy var inputBarView: InputBarView = {
        let inputBarView = InputBarView()
        inputBarView.delegate = self
        return inputBarView
    }()

    private lazy var backgroundView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = inputBarView.backgroundColor
        return view
    }()
    
    private var emotionBoardViewHeight: CGFloat = 274.scale375Height()

    private lazy var emotionBoardView: EmotionBoardView = {
        let emotionBoardView = EmotionBoardView()
        let emotionHelper = EmotionHelper.shared
        emotionHelper.useDefaultEmotions()
        emotionBoardView.emotions = emotionHelper.emotions
        emotionBoardView.delegate = self
        emotionBoardView.backgroundColor = UIColor(red: 34 / 255.0, green: 38 / 255.0, blue: 46 / 255.0, alpha: 1)
        emotionBoardView.setDeleteBtnEnable(!BarrageManager.shared.inputString.isEmpty)
        return emotionBoardView
    }()
    
    private var currentInputState: InputState = .none {
        didSet {
            switch currentInputState {
            case .text:
                inputBarView.becomeFirstResponder()
            default:
                inputBarView.resignFirstResponder()
            }
        }
    }
    
    private var isShowEmotion: Bool = false
    
    init(roomId: String, isShowEmotion: Bool = false) {
        self.roomId = roomId
        self.isShowEmotion = isShowEmotion
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        
        guard let touch = touches.first else { return }
        let touchPoint = touch.location(in: view)
        if backgroundView.frame.contains(touchPoint) {
            return
        }
        
        close()
    }
    
    deinit {
        cancellableSet.forEach { $0.cancel() }
        removeObserver()
        debugPrint("deinit:\(self)")
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        view.backgroundColor = .clear
        
        addObserver()
        
        constructViewHierarchy()
        activateConstraints()
        
        liveListStore.liveListEventPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] event in
                guard let self = self else { return }
                if case .onLiveEnded(liveId: let liveId, reason: _, message: _) = event,
                   liveId == roomId {
                    dismiss(animated: true)
                }
            }
            .store(in: &cancellableSet)
        
        initInputBarStatus()
    }
    
    private func close() {
        currentInputState = .none
        dismiss(animated: true, completion: nil)
    }
    
    private func constructViewHierarchy() {
        view.addSubview(backgroundView)
        view.addSubview(emotionBoardView)
        view.addSubview(inputBarView)
    }

    private func activateConstraints() {
        emotionBoardView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.bottom.equalTo(view.safeAreaLayoutGuide.snp.bottom)
            make.height.equalTo(emotionBoardViewHeight)
        }
        
        inputBarView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.bottom.equalTo(view.safeAreaLayoutGuide.snp.bottom).offset(-emotionBoardViewHeight)
        }
        
        backgroundView.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(inputBarView)
        }
    }
    
    private func initInputBarStatus() {
        currentInputState = isShowEmotion ? .emotion : .text
        inputBarView.emotionSwitchButton.isSelected = isShowEmotion
    }
}


// MARK: - Keyboard listener
extension BarrageSendViewController {
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
        onKeyboardStatusChanged(isShow: true, frame: keyboardRect, animationOptions: animationOptionsForCurve(curve: curve), duration: duration)
    }

    @objc func onKeyboardWillHide(notification: Notification) {
        guard let keyboardRect = notification.userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? CGRect,
              let curveRawValue = notification.userInfo?[UIResponder.keyboardAnimationCurveUserInfoKey] as? Int,
              let duration = notification.userInfo?[UIResponder.keyboardAnimationDurationUserInfoKey] as? Double,
              let curve = UIView.AnimationCurve(rawValue: curveRawValue)
        else { return }
        onKeyboardStatusChanged(isShow: false, frame: keyboardRect, animationOptions: animationOptionsForCurve(curve: curve), duration: duration)
    }
    
    private func onKeyboardStatusChanged(isShow: Bool, frame: CGRect, animationOptions: UIView.AnimationOptions, duration: Double) {
        UIView.animate(withDuration: duration, delay: 0, options: animationOptions) { [weak self] in
            guard let self = self else { return }
            if isShow {
                inputBarView.transform = CGAffineTransform(translationX: 0, y: emotionBoardViewHeight + WindowUtils.bottomSafeHeight - frame.height)
                emotionBoardView.transform = CGAffineTransform(translationX: 0, y: CGRectGetHeight(emotionBoardView.frame))
            } else {
                inputBarView.transform = .identity
                emotionBoardView.transform = .identity
            }
            backgroundView.transform = inputBarView.transform
        }
    }

    private func animationOptionsForCurve(curve: UIView.AnimationCurve) -> UIView.AnimationOptions {
        return UIView.AnimationOptions(rawValue: UInt(curve.rawValue))
    }
}

// MARK: - InputBarViewDelegate
extension BarrageSendViewController: InputBarViewDelegate {
    func inputBarView(inputBarView: InputBarView, onSendText inputNormalText: String) {
        barrageStore.sendTextMessage(text: inputNormalText, extensionInfo: nil) { result in
            switch result {
            case .success(()):
                BarrageManager.shared.inputString = ""
            case .failure(let errorInfo):
                let error = InternalError(errorInfo: errorInfo)
                BarrageManager.shared.toastSubject.send(error.localizedMessage)
            }
        }
        close()
    }
    
    func inputBarViewDidSwitchToEmotion(isEmotion: Bool) {
        if isEmotion {
            currentInputState = .emotion
            UIView.animate(withDuration: 0.2, delay: 0, options: UIView.AnimationOptions.curveEaseInOut) { [weak self] in
                guard let self = self else { return }
                inputBarView.transform = .identity
                backgroundView.transform = .identity
                emotionBoardView.transform = .identity
            }
        } else {
            currentInputState = .text
        }
    }
    
    func inputBarEmptyChanged(isEmpty: Bool) {
        emotionBoardView.setDeleteBtnEnable(!isEmpty)
    }
}

extension BarrageSendViewController {
    var barrageStore: BarrageStore {
        return BarrageStore.create(liveId: roomId)
    }
    
    var liveListStore: LiveListStore {
        return LiveListStore.shared
    }
}

// MARK: - EmotionBoardViewDelegate
extension BarrageSendViewController: EmotionBoardViewDelegate {
    func emotionView(emotionBoardView: EmotionBoardView, didSelectEmotion emotion: Emotion, atIndex index: Int) {
        let emotionHelper = EmotionHelper.shared
        let attributedString = emotionHelper.obtainImageAttributedString(byImageKey: emotion.displayName,
                                                                         font: inputBarView.inputTextView.font ?? UIFont(), 
                                                                         useCache: false)
        inputBarView.insert(emotionImageAttributedString: attributedString)
    }

    func emotionViewDidSelectDeleteButton(emotionBoardView: EmotionBoardView) {
        if !inputBarView.deleteEmotion() {
            inputBarView.inputTextView.deleteBackward()
        }
    }
}
