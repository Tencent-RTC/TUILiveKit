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
import TUIGift

protocol GiftListPanelProvider {
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
    private let provider: GiftListPanelProvider
    private var cancellableSet: Set<AnyCancellable> = []

    init(roomId: String, provider: GiftListPanelProvider) {
        self.roomId = roomId
        self.provider = provider
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
    
    func setGiftList(_ giftList: [TUIGift]) {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            giftListView.setGiftList(giftList)
        }
    }
    
    func sendGift(giftModel: TUIGift, giftCount: Int, receiver: TUIGiftUser, completion: TUIGiftIMSendBlock) {
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
            make.height.equalTo(256)
            make.top.equalTo(titleLabel.snp.bottom).offset(32)
            make.bottom.trailing.equalToSuperview().offset(-16)
        }
    }
}

// MARK: GiftListViewDelegate

extension GiftListPanel: GiftListViewDelegate {
    func onSendGift(gift model: TUIGift, giftCount: Int) {
        let anchorInfo = provider.getAnchorInfo()
        let receiver = TUIGiftUser()
        receiver.userId = anchorInfo.userId
        receiver.userName = anchorInfo.name
        receiver.avatarUrl = anchorInfo.avatarUrl
        receiver.level = "0"
        
        sendGift(giftModel: model, giftCount: giftCount, receiver: receiver) { [weak self] code, message in
            guard let self = self, code != 0 else { return }
            if code == 10017 {
                makeToast(.giftMutedText)
            } else {
                let err = InternalError(code: code, message: message)
                makeToast(err.localizedMessage)
            }
        }
    }
}

private extension String {
    static let giftTitle = localized("Gift")
    static let giftMutedText = localized("You have been muted in the current room and cannot send gifts")
}
