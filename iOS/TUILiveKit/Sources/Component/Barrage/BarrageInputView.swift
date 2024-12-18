//
//  BarrageInputView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import TUICore
import UIKit
import RTCCommon

protocol BarrageInputViewDelegate: AnyObject {
    func barrageInputViewOnSendBarrage(_ barrage: TUIBarrage)
}

class BarrageInputView: UIView {
    
    weak var delegate: BarrageInputViewDelegate?
    
    private let roomId: String
    private var defaultView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        return view
    }()

    private lazy var emojiView: UIImageView = {
        let view = UIImageView()
        view.image = UIImage(named: "live_emoji_icon", in: Bundle.liveBundle, compatibleWith: nil)
        view.isUserInteractionEnabled = false
        return view
    }()

    private lazy var placeholderLabel: UILabel = {
        let label = UILabel()
        label.backgroundColor = .clear
        label.textColor = .g8
        label.font = UIFont(name: "PingFangSC-Regular", size: 12)
        label.text = .chatText
        return label
    }()

    init(roomId: String) {
        self.roomId = roomId
        super.init(frame: .zero)
        backgroundColor = .g2.withAlphaComponent(0.5)
        layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        layer.borderWidth = 0.5
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else {
            return
        }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        
        let vc = BarrageSendViewController(roomId: roomId)
        vc.modalPresentationStyle = .overFullScreen
        vc.delegate = self
        WindowUtils.getCurrentWindowViewController()?.present(vc, animated: true)
    }
}

extension BarrageInputView: BarrageSendViewControllerDelegate {
    func barrageSendViewControllerOnSendBarrage(_ barrage: TUIBarrage) {
        delegate?.barrageInputViewOnSendBarrage(barrage)
    }
}

// MARK: - Special configure
extension BarrageInputView {
    func setText(text: String) {
        placeholderLabel.text = text
    }
    
    func setTextColor(color: UIColor) {
        placeholderLabel.textColor = color
    }
    
    func setImage(image: UIImage) {
        emojiView.image = image
    }
    
    func setTextVisibility(show: Bool) {
        placeholderLabel.isHidden = !show
    }
}

// MARK: - Private functions
extension BarrageInputView {
    private func constructViewHierarchy() {
        addSubview(emojiView)
        addSubview(placeholderLabel)
    }

    private func activateConstraints() {
        emojiView.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-8.scale375())
            make.centerY.equalToSuperview()
            make.width.equalTo(20.scale375Width())
            make.height.equalTo(20.scale375Width())
        }

        placeholderLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(8.scale375())
            make.centerY.equalToSuperview()
            make.trailing.equalTo(emojiView.snp.leading).offset(-6.scale375())
            make.height.equalTo(24.scale375Width())
        }
    }
}

private extension String {
    static let chatText = localized("live.audience.barrage.placeholder")
}
