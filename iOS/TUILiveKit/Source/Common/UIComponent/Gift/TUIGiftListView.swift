//
//  TUIGiftListView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/7.
//

import Foundation
import SnapKit
import TUICore

protocol TUIGiftListViewDelegate: AnyObject {
    func onRecharge(giftListView view: TUIGiftListView)
    func onSendGift(giftListView view: TUIGiftListView, giftModel: TUIGift, giftCount: Int)
}

class TUIGiftListView: UIView {
    private var giftArray: [TUIGift] = []
    private var groupId: String
    weak var delegate: TUIGiftListViewDelegate?
    init(groupId: String) {
        self.groupId = groupId
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

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

    private lazy var giftListPanelView: TUIGiftPanelView = {
        TUIGiftPanelView(self, groupId: self.groupId)
    }()
    
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

    private func updateGiftListView() {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.giftListPanelView.setGiftModelSource(self.giftArray)
            self.giftListPanelView.reloadData()
        }
    }
    
    @objc func rechargeButtonClick() {
        delegate?.onRecharge(giftListView: self)
    }
    
    func setBalance(_ balance: Int) {
        self.balanceLabel.text = .localizedReplace(.balanceText, replace: "\(balance)")
    }
    
    func setGiftList(_ giftList: [TUIGift]) {
        giftArray = giftList
        updateGiftListView()
    }
    
    func sendGift(giftModel: TUIGift, giftCount: Int, receiver: TUIGiftUser) {
        giftListPanelView.sendGift(model: giftModel, giftCount: giftCount, receiver: receiver)
    }
    
    func sendLike() {
        giftListPanelView.sendLike()
    }
    
    func setRoomId(roomId: String) {
        self.groupId = roomId
        giftListPanelView = TUIGiftPanelView(self, groupId: self.groupId)
    }
}


// MARK: Layout

extension TUIGiftListView {
    private func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        addSubview(titleLabel)
        addSubview(giftListPanelView)
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

        giftListPanelView.snp.remakeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.bottom.equalTo(rechargeButton.snp.top).offset(-12)
            make.height.equalTo(256)
            make.top.equalTo(titleLabel.snp.bottom).offset(32)
        }
        
        rechargeButton.snp.remakeConstraints { make in
            make.top.equalTo(giftListPanelView.snp.bottom).offset(12)
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

extension TUIGiftListView: TUIGiftPanelDelegate {
    func onSendGift(gift model: TUIGift, giftCount: Int) {
        delegate?.onSendGift(giftListView: self, giftModel: model, giftCount: giftCount)
    }
    
    func onGiftDidSend(_ giftView: TUIGiftPanelView,
                       gift model: TUIGift,
                       sender: TUIGiftUser,
                       receiver: TUIGiftUser,
                       giftCount: Int,
                       isSuccess: Bool,
                       message: String) {
        if !isSuccess {
            superview?.makeToast(message)
        }
    }
   
    func onLikeDidSend(_ giftView: TUIGiftPanelView, sender: TUIGiftUser, isSuccess: Bool, message: String) {
        TUIGiftStore.shared.likeData.value = TUILikeData(sender: sender)
    }
}

private extension String {
    static var giftTitle: String {
        localized("live.audience.gift.title")
    }
    static var meText: String {
        localized("live.barrage.me")
    }
    static var sendText: String {
        localized("live.giftView.sendOut")
    }
    static var balanceText: String {
        localized("live.giftView.balance.xxx")
    }
    static var rechargeText: String {
        localized("live.giftView.recharge")
    }
}
