//
//  GiftListPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/7.
//

import Foundation
import SnapKit
import TUICore
import RTCCommon
import Combine

protocol GiftListPanelDataSource {
    func getAnchorInfo() -> GiftUser
}

struct GiftUser {
    var userId: String = ""
    var name: String = ""
    var avatarUrl: String = ""
    init(userId: String, name: String, avatarUrl: String) {
        self.userId = userId
        self.name = name
        self.avatarUrl = avatarUrl
    }
}

class GiftListPanel: UIView {
    private let roomId: String
    private let dataSource: GiftListPanelDataSource
    private var cancellableSet: Set<AnyCancellable> = []

    init(roomId: String, dataSource: GiftListPanelDataSource) {
        self.roomId = roomId
        self.dataSource = dataSource
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .white
        label.text = .giftTitle
        label.sizeToFit()
        return label
    }()

    private lazy var giftListView = GiftListView(roomId: roomId, delegate: self)
    
    private lazy var balanceLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 14, weight: .medium)
        label.textColor = .white
        label.text = .localizedReplace(.balanceText, replace: "0")
        label.sizeToFit()
        return label
    }()
    
    private lazy var rechargeButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = .systemBlue
        button.setTitle(.rechargeText, for: .normal)
        button.setTitleColor(.white, for: .normal)
        button.layer.cornerRadius = 12
        button.contentEdgeInsets = UIEdgeInsets(top: 0, left: 6, bottom: 0, right: 6)
        button.addTarget(self, action: #selector(rechargeButtonClick), for: .touchUpInside)
        return button
    }()
    
    @objc func rechargeButtonClick() {
        TUIGiftStore.shared.giftCloudServer.rechargeBalance { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                setBalance(balance)
            } else {
                makeToast(.balanceInsufficientText)
            }
        }
    }
    
    func setBalance(_ balance: Int) {
        self.balanceLabel.text = .localizedReplace(.balanceText, replace: "\(balance)")
    }
    
    func setGiftList(_ giftList: [TUIGift]) {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            TUIGiftStore.shared.giftCloudServer.queryBalance { [weak self] error, balance in
                guard let self = self else { return }
                if error == .noError {
                    setBalance(balance)
                }
            }
            self.giftListView.setGiftList(giftList)
        }
    }
    
    func sendGift(giftModel: TUIGift, giftCount: Int, receiver: TUIGiftUser, completion: @escaping (Bool, String) -> ()) {
        giftListView.sendGift(model: giftModel, giftCount: giftCount, receiver: receiver, completion: completion)
    }
}

// MARK: Layout

extension GiftListPanel {
    private func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        addSubview(titleLabel)
        addSubview(giftListView)
        addSubview(balanceLabel)
        addSubview(rechargeButton)
    }

    private func activateConstraints() {
        titleLabel.snp.remakeConstraints { make in
            make.top.equalToSuperview().offset(20)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375Width())
            make.width.equalTo(titleLabel.mm_w)
        }

        giftListView.snp.remakeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.bottom.equalTo(rechargeButton.snp.top).offset(-12)
            make.height.equalTo(256)
            make.top.equalTo(titleLabel.snp.bottom).offset(32)
        }
        
        rechargeButton.snp.remakeConstraints { make in
            make.top.equalTo(giftListView.snp.bottom).offset(12)
            make.bottom.trailing.equalToSuperview().offset(-16)
            make.height.equalTo(24)
        }
        
        balanceLabel.snp.remakeConstraints { make in
            make.bottom.equalTo(rechargeButton.snp.bottom)
            make.trailing.equalTo(rechargeButton.snp.leading).offset(-12)
            make.height.equalTo(24)
        }
    }
}

// MARK: GiftListViewDelegate

extension GiftListPanel: GiftListViewDelegate {
    func onSendGift(gift model: TUIGift, giftCount: Int) {
        let anchorInfo = dataSource.getAnchorInfo()
        let receiver = TUIGiftUser()
        receiver.userId = anchorInfo.userId
        receiver.userName = anchorInfo.name
        receiver.avatarUrl = anchorInfo.avatarUrl
        receiver.level = "0"
        
        TUIGiftStore.shared.giftCloudServer.sendGift(sender: TUILogin.getUserID() ?? "",
                                                     receiver: receiver.userId,
                                                     giftModel: model,
                                                     giftCount: giftCount) { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                sendGift(giftModel: model, giftCount: giftCount, receiver: receiver) { [weak self] isSuccess, message in
                    guard let self = self else { return }
                    if !isSuccess {
                        makeToast(message)
                    }
                }
                setBalance(balance)
            } else {
                makeToast(.balanceInsufficientText)
            }
        }
    }
}

private extension String {
    static let giftTitle = localized("live.audience.gift.title")
    static let meText = localized("live.barrage.me")
    static let sendText = localized("live.giftView.sendOut")
    static let balanceText = localized("live.giftView.balance.xxx")
    static let rechargeText = localized("live.giftView.recharge")
    static let balanceInsufficientText = localized("live.balanceInsufficient")
}
