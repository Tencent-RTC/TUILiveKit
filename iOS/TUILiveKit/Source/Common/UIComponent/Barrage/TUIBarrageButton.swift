//
// TUIBarrageSendView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import TUICore
import UIKit
import RTCCommon

class TUIBarrageButton: UIView {
    private var roomId: String
    private lazy var barrageManager:TUIBarrageManager = {
       TUIBarrageManager.defaultCreate(roomId: roomId, delegate: self)
    }()

    private var defaultView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        return view
    }()

    private let emojiView: UIImageView = {
        var view = UIImageView()
        view.image = UIImage(named: "live_emoji_icon", in: Bundle.liveBundle, compatibleWith: nil)
        return view
    }()

    private let label: UILabel = {
        var label = UILabel()
        label.backgroundColor = .clear
        label.textColor = .g8
        label.font = UIFont(name: "PingFangSC-Regular", size: 12)
        label.text = .chatText
        return label
    }()

    private lazy var clickView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        let tap = UITapGestureRecognizer(target: self, action: #selector(showInputView))
        view.addGestureRecognizer(tap)
        view.isUserInteractionEnabled = true
        return view
    }()

    init(roomId: String, ownerId: String) {
        self.roomId = roomId
        TUIBarrageStore.shared.ownerId = ownerId
        super.init(frame: .zero)
        barrageManager.initService()
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

    private func constructViewHierarchy() {
        addSubview(emojiView)
        addSubview(label)
        addSubview(clickView)
    }

    private func activateConstraints() {
        emojiView.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-8.scale375())
            make.centerY.equalToSuperview()
            make.width.equalTo(20.scale375Width())
            make.height.equalTo(20.scale375Width())
        }

        label.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(8.scale375())
            make.centerY.equalToSuperview()
            make.trailing.equalTo(emojiView.snp.leading).offset(-6.scale375())
            make.height.equalTo(24.scale375Width())
        }

        clickView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }

    @objc private func showInputView() {
        let barrageInputViewController = BarrageSendViewController(delegate: self)
        barrageInputViewController.view.backgroundColor = .clear
        let nav = UINavigationController(rootViewController: barrageInputViewController)
        nav.modalPresentationStyle = .overFullScreen
        WindowUtils.getCurrentWindowViewController()?.present(nav, animated: true)
        barrageInputViewController.showInputBar()
    }
    
    private func sendBarrage(_ barrage: TUIBarrage) {
        barrageManager.sendBarrage(barrage)
    }
    
    func setRoomId(roomId: String) {
        self.roomId = roomId
        barrageManager = TUIBarrageManager.defaultCreate(roomId: roomId, delegate: self)
        barrageManager.initService()
    }
    
    func setOwnerId(ownerId: String) {
        TUIBarrageStore.shared.ownerId = ownerId
    }
    
    func setText(text: String) {
        label.text = text
    }
    
    func setTextColor(color: UIColor) {
        label.textColor = color
    }
    
    func setImage(image: UIImage) {
        emojiView.image = image
    }
    
    func setTextVisibility(show: Bool) {
        label.isHidden = !show
    }
}

extension TUIBarrageButton: BarrageSendViewControllerDelegate {
    func barrageSendViewController(_ barrageSendViewController: BarrageSendViewController, message: String) {
        let barrage = TUIBarrage()
        barrage.content = message
        sendBarrage(barrage)
    }
}

extension TUIBarrageButton: TUIBarrageManagerDelegate {
    func willSendBarrage(_ barrage: TUIBarrage) {
        let userId = TUILogin.getUserID() ?? ""
        barrage.user.userName = TUILogin.getNickName() ?? userId
        barrage.user.userId = userId
        barrage.user.avatarUrl = TUILogin.getFaceUrl() ?? ""
        barrage.user.level = "0"
    }

    func didSendBarrage(_ barrage: TUIBarrage) {}

    func didReceiveBarrage(_ barrage: TUIBarrage) {}
}

private extension String {
    static let chatText = localized("live.audience.barrage.placeholder")
}
