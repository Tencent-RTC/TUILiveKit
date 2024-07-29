//
//  AudienceLivingView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/12/15.
//

import TUICore
import UIKit
import RTCCommon
import Combine

class AudienceLivingView: RTCBaseView {
    // MARK: - private property.
    private let store: LiveStore
    private let viewStore: LiveRoomViewStore
    private let routerStore: RouterStore
    
    private lazy var roomIdPublisher = store.select(RoomSelectors.getRoomId)
    private lazy var ownerInfoPublisher = store.select(RoomSelectors.getRoomOwnerInfo)
    private var cancellableSet = Set<AnyCancellable>()
    private let giftCacheService = TUIGiftStore.shared.giftCacheService

    private lazy var roomInfoView: RoomInfoView = {
        let view = RoomInfoView(store: store)
        view.mm_h = 32.scale375()
        view.backgroundColor = UIColor.g1.withAlphaComponent(0.4)
        view.layer.cornerRadius = view.mm_h * 0.5
        view.addTarget(self, action: #selector(roomInfoViewClick), for: .touchUpInside)
        return view
    }()

    private lazy var audienceListView: AudienceListView = {
        var view = AudienceListView(store: store, routerStore: routerStore)
        return view
    }()

    private lazy var leaveImageView: UIImageView = {
        var imageView = UIImageView()
        imageView.image = .liveBundleImage("live_leave_icon")
        let leaveTap = UITapGestureRecognizer(target: self, action: #selector(leaveImageViewClick))
        imageView.isUserInteractionEnabled = true
        imageView.addGestureRecognizer(leaveTap)
        return imageView
    }()

    private lazy var barrageSendView: TUIBarrageButton = {
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        let ownerId = store.selectCurrent(RoomSelectors.roomOwnerId)
        var view = TUIBarrageButton(roomId: roomId, ownerId: ownerId)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 0.5
        view.layer.cornerRadius = 18.scale375Height()
        view.layer.masksToBounds = true
        return view
    }()

    private lazy var bottomMenu: BottomMenuView = {
        let view = BottomMenuView(frame: .zero)
        return view
    }()

    private lazy var floatView: LinkMicAudienceFloatView = {
        let view = LinkMicAudienceFloatView(store: store, routerStore: routerStore)
        view.isHidden = true
        return view
    }()

    private lazy var barrageDisplayView: TUIBarrageDisplayView = {
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        let ownerId = store.selectCurrent(RoomSelectors.roomOwnerId)
        let view = TUIBarrageDisplayView(roomId: roomId, ownerId: ownerId)
        view.delegate = self
        return view
    }()

    private lazy var giftDisplayView: TUIGiftPlayView = {
        let view = TUIGiftPlayView(groupId: store.roomState.roomId)
        view.delegate = self
        return view
    }()
    
    init(store: LiveStore, viewStore: LiveRoomViewStore, routerStore: RouterStore) {
        self.store = store
        self.viewStore = viewStore
        self.routerStore = routerStore
        super.init(frame: .zero)
    }

    override func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(barrageDisplayView)
        addSubview(giftDisplayView)
        addSubview(roomInfoView)
        addSubview(audienceListView)
        addSubview(leaveImageView)
        addSubview(bottomMenu)
        addSubview(floatView)
        addSubview(barrageSendView)
    }

    override func activateConstraints() {
        giftDisplayView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }

        barrageDisplayView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16)
            make.bottom.equalTo(barrageSendView.snp.top).offset(-8.scale375Height())
            make.width.equalTo(247.scale375Width())
            make.height.equalTo(212.scale375Height())
        }

        roomInfoView.snp.remakeConstraints { make in
            make.top.equalToSuperview().offset(54.scale375Height())
            make.height.equalTo(roomInfoView.frame.height)
            make.width.greaterThanOrEqualTo(80.scale375())
            make.width.lessThanOrEqualTo(375.scale375()*0.5)
            make.leading.equalToSuperview().inset(16.scale375())
        }

        leaveImageView.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-20.scale375Width())
            make.top.equalToSuperview().offset(58.scale375Height())
            make.width.equalTo(24.scale375Width())
            make.height.equalTo(24.scale375Width())
        }

        audienceListView.snp.makeConstraints { make in
            make.trailing.equalTo(leaveImageView.snp.leading).offset(-4.scale375Width())
            make.centerY.equalTo(leaveImageView)
            make.height.equalTo(24.scale375())
            make.width.equalTo(116.scale375())
        }
        
        barrageSendView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16.scale375())
            make.width.equalTo(130.scale375())
            make.height.equalTo(36.scale375Height())
            make.bottom.equalToSuperview().offset(-34.scale375Height())
        }

        bottomMenu.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-16.scale375Width())
            make.height.equalTo(36.scale375Height())
            make.bottom.equalToSuperview().offset(-34.scale375Height())
        }

        floatView.snp.makeConstraints { make in
            make.top.equalTo(audienceListView.snp.bottom).offset(34.scale375Width())
            make.height.width.equalTo(86.scale375())
            make.trailing.equalToSuperview().offset(-8.scale375())
        }
    }
    
    override func bindInteraction() {
        subscribeBottomMenuState()
        subscribeSeatSubject()
    }
}

extension AudienceLivingView {
    
    private func subscribeBottomMenuState() {
        viewStore.select(LiveRoomViewSelectors.getBottomMenuButtons)
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] menus in
                guard let self = self else { return }
                self.bottomMenu.menus = menus
            })
            .store(in: &cancellableSet)
    }
    
    private func subscribeSeatSubject() {

        roomIdPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] roomId in
                guard let self = self else { return }
                self.barrageSendView.setRoomId(roomId: roomId)
                self.barrageDisplayView.setRoomId(roomId: roomId)
                self.giftDisplayView.setRoomId(roomId: roomId)
            }
            .store(in: &cancellableSet)
        
        ownerInfoPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.barrageSendView.setOwnerId(ownerId: ownerInfo.userId)
                self.barrageDisplayView.setOwnerId(ownerId: ownerInfo.userId)
            }
            .store(in: &cancellableSet)
        
        store.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] toastInfo in
                guard let self = self else { return }
                self.makeToast(toastInfo.message)
            }
            .store(in: &cancellableSet)
    }
}

// MARK: - View Action.
extension AudienceLivingView {
    @objc func roomInfoViewClick() {
        routerStore.router(action: RouterAction.present(.roomInfo))
    }

    @objc func leaveImageViewClick() {
        store.dispatch(action: RoomActions.leave())
        routerStore.router(action: RouterAction.exit)
    }

    private func showCancelLinkMicPanel() {
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redPinkColor)
        designConfig.backgroundColor = .g2
        designConfig.lineColor = .g3.withAlphaComponent(0.1)
        let item = ActionItem(title: .cancelLinkMicRequestText, designConfig: designConfig) { [weak self] _ in
            guard let self = self else { return }
            let requestId = self.store.selectCurrent(SeatSelectors.getMySeatApplicationId)
            self.store.dispatch(action: SeatActions.cancelApplication(payload: requestId))
            self.routerStore.router(action: .dismiss)
        }
        routerStore.router(action: RouterAction.present(.listMenu([item])))
    }
}

extension AudienceLivingView: TUIBarrageDisplayViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: TUIBarrageDisplayView, createCustomCell barrage: TUIBarrage) -> UIView? {
        guard let type = barrage.extInfo["TYPE"], type.value as? String == "GIFTMESSAGE" else {
            return nil
        }
        return CustomBarrageCell.getCustomCell(barrage: barrage)
    }
}

extension AudienceLivingView: TUIGiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        let barrage = TUIBarrage()
        barrage.content = "gift"
        barrage.user.userId = sender.userId
        barrage.user.userName = sender.userName
        barrage.user.avatarUrl = sender.avatarUrl
        barrage.user.level = sender.level
        barrage.extInfo["TYPE"] = AnyCodable("GIFTMESSAGE")
        barrage.extInfo["gift_name"] = AnyCodable(gift.giftName)
        barrage.extInfo["gift_count"] = AnyCodable(giftCount)
        barrage.extInfo["gift_icon_url"] = AnyCodable(gift.imageUrl)
        if receiver.userId == TUILogin.getUserID() {
            receiver.userName = .meText
        }
        barrage.extInfo["gift_receiver_username"] = AnyCodable(receiver.userName)
        barrageDisplayView.insertBarrages([barrage])
    }
    
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onPlayGiftAnimation gift: TUIGift) {
        guard let url = URL(string: gift.animationUrl) else { return }
        giftCacheService.request(withURL: url) { error, fileUrl in
            if error == 0 {
                DispatchQueue.main.async {
                    giftPlayView.playGiftAnimation(playUrl: fileUrl)
                }
            }
        }
    }
}

private extension String {
    static let chatPlaceHolderText = localized("live.audience.barrage.placeholder")
    static let cancelLinkMicRequestText = localized("live.audience.link.confirm.cancelLinkMicRequest")
    static let closeLinkMicText = localized("live.audience.link.confirm.closeLinkMic")
    static let waitToLinkText = localized("live.audience.wait.link.tips")
    static let enterRoomFailedTitleText = localized("live.alert.enterRoom.failed.title")
    static let enterRoomFailedmessageText = localized("live.alert.enterRoom.failed.message.xxx")
    static let confirmText = localized("live.alert.confirm")
    static let rejectedTitleText = localized("live.alert.linkMic.rejected.title")
    static let knownText = localized("live.alert.known")
    static let timeoutTitleText = localized("live.alert.linkMic.timeout.title")
    static let operateFailedText = localized("live.operation.fail.xxx")
    static let meText = localized("live.barrage.me")
}
