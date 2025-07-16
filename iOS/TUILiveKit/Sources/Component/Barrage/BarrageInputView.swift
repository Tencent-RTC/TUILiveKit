//
//  BarrageInputView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import TUICore
import UIKit
import RTCCommon

public class BarrageInputView: UIView {
        
    private let roomId: String
    private var defaultView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        return view
    }()

    private lazy var emojiButton: UIButton = {
        let button = UIButton()
        button.setImage(internalImage("live_emoji_icon")?.withTintColor(.white.withAlphaComponent(0.55)), for: .normal)
        button.addTarget(self, action: #selector(emojiButtonTapped), for: .touchUpInside)
        return button
    }()

    private lazy var placeholderLabel: UILabel = {
        let label = UILabel()
        label.backgroundColor = .clear
        label.textColor = .white.withAlphaComponent(0.55)
        label.font = .customFont(ofSize: 12, weight: .semibold)
        label.text = .chatText
        return label
    }()

    public init(roomId: String) {
        self.roomId = roomId
        super.init(frame: .zero)
        backgroundColor = .black.withAlphaComponent(0.25)
        layer.borderColor = UIColor.white.withAlphaComponent(0.14).cgColor
        layer.borderWidth = 1
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        presentSendVC()
    }
    
    @objc private func emojiButtonTapped() {
        presentSendVC(isShowEmotion: true)
    }
    
    private func presentSendVC(isShowEmotion: Bool = false) {
        let vc = BarrageSendViewController(roomId: roomId, isShowEmotion: isShowEmotion)
        vc.modalPresentationStyle = .overFullScreen
        WindowUtils.getCurrentWindowViewController()?.present(vc, animated: true)
    }
}

// MARK: - Special configure
extension BarrageInputView {
    public func setText(text: String) {
        placeholderLabel.text = text
    }
    
    public func setTextColor(color: UIColor) {
        placeholderLabel.textColor = color
    }
    
    public func setImage(image: UIImage) {
        emojiButton.setImage(image, for: .normal)
    }
    
    public func setTextVisibility(show: Bool) {
        placeholderLabel.isHidden = !show
    }
}

// MARK: - Private functions
extension BarrageInputView {
    private func constructViewHierarchy() {
        addSubview(emojiButton)
        addSubview(placeholderLabel)
    }

    private func activateConstraints() {
        emojiButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-8.scale375())
            make.centerY.equalToSuperview()
            make.width.equalTo(20.scale375Width())
            make.height.equalTo(20.scale375Width())
        }

        placeholderLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(12.scale375())
            make.centerY.equalToSuperview()
            make.trailing.equalTo(emojiButton.snp.leading).offset(-6.scale375())
            make.height.equalTo(24.scale375Width())
        }
    }
}

private extension String {
    static let chatText = internalLocalized("Join Chat!")
}
