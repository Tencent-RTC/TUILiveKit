//
//  AudienceView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/10/19.
//

import Foundation
import RTCCommon
import Combine
import LiveStreamCore
import RTCRoomEngine

class AudienceView: RTCBaseView {
    let roomId: String
    // MARK: - private property
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    private var cancellableSet: Set<AnyCancellable> = []
    
    // MARK: - property: view
    private let videoView: LiveCoreView
    
    lazy var livingView: AudienceLivingView = {
        let view = AudienceLivingView(manager: manager, routerManager: routerManager, coreView: videoView)
        return view
    }()
    
    private lazy var battleInfoView: BattleInfoView = {
        let view = BattleInfoView(manager: manager, routerManager: routerManager, isOwner: false)
        return view
    }()
    
    lazy var dashboardView: AudienceEndView = {
        let roomOwner = manager.roomState.ownerInfo
        let view = AudienceEndView(roomId: roomId, avatarUrl: roomOwner.avatarUrl, userName: roomOwner.name)
        view.delegate = self
        view.isHidden = true
        return view
    }()
    
    lazy var giftPanelView: GiftListView = {
        let view = GiftListView(groupId: roomId)
        view.delegate = self
        TUIGiftStore.shared.giftCloudServer.queryBalance { error, balance in
            if error == .noError {
                view.setBalance(balance)
            }
        }
        return view
    }()
    
    lazy var beautyPanelView: UIView = {
        let view = BeautyView(roomId: roomId, isOwner: false)
        view.backClosure = { [weak self] in
            guard let self = self else { return }
            routerManager.router(action: .dismiss())
        }
        return view
    }()
    
    init(roomId: String, manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) {
        self.roomId = roomId
        self.manager = manager
        self.routerManager = routerManager
        self.videoView = coreView
        super.init(frame: .zero)
        self.videoView.videoViewDelegate = self
        self.videoView.waitingCoGuestViewDelegate = self
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(self)")
    }
    
    override func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(videoView)
        addSubview(battleInfoView)
        addSubview(livingView)
        addSubview(dashboardView)
    }
    
    override func activateConstraints() {
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        battleInfoView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        livingView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        dashboardView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    override func bindInteraction() {
        prepareRoomState()
        subscribeRoomState()
        subscribeCustomEvent()
    }
    
    private func prepareRoomState() {
        manager.update { roomState in
            roomState.roomId = roomId
        }
        startDisplay()
    }
    
    func relayoutCoreView() {
        addSubview(videoView)
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        sendSubviewToBack(videoView)
    }
}

extension AudienceView {
    private func subscribeCustomEvent() {
        manager.likeSubject
            .sink { [weak self] in
                self?.giftPanelView.sendLike()
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeRoomState() {
        manager.subscribeRoomState(StateSelector(keyPath: \LSRoomState.liveStatus))
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .none,.previewing:
                        // TODO: - mute all?
                        break
                    case .finished:
                        dashboardView.update(avatarUrl: manager.roomState.ownerInfo.avatarUrl,
                                             userName: manager.roomState.ownerInfo.name)
                        dashboardView.isHidden = false
                    case .playing:
                        self.didEnterRoom()
                        break
                    case .pushing:
                        break
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func didEnterRoom() {
        manager.fetchSeatList()
    }
}

extension AudienceView: LSRouterViewProvider {
    func getRouteView(route: LSRoute) -> UIView? {
        if route == .giftView {
            giftPanelView.setGiftList(TUIGiftStore.shared.giftList)
            return giftPanelView
        } else if route == .beauty {
            return beautyPanelView
        }
        else {
            return nil
        }
    }
}

extension AudienceView: GiftListViewDelegate {
    func onRecharge(giftListView view: GiftListView) {
        TUIGiftStore.shared.giftCloudServer.rechargeBalance { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.setBalance(balance)
            } else {
                manager.toastSubject.send(.balanceInsufficientText)
            }
        }
    }
    
    func onSendGift(giftListView view: GiftListView, giftModel: TUIGift, giftCount: Int) {
        let anchorInfo = manager.roomState.ownerInfo
        let receiver = TUIGiftUser()
        receiver.userId = anchorInfo.userId
        receiver.userName = anchorInfo.name
        receiver.avatarUrl = anchorInfo.avatarUrl
        receiver.level = "0"
        
        let selfInfo = manager.userState.selfInfo
        TUIGiftStore.shared.giftCloudServer.sendGift(sender: selfInfo.userId,
                                                     receiver: receiver.userId,
                                                     giftModel: giftModel,
                                                     giftCount: giftCount) { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.sendGift(giftModel: giftModel, giftCount: giftCount, receiver: receiver)
                view.setBalance(balance)
            } else {
                manager.toastSubject.send(.balanceInsufficientText)
            }
        }
    }
}

// MARK: - LiveEndViewDelegate
extension AudienceView: LiveEndViewDelegate {
    func onCloseButtonClick() {
        routerManager.router(action: .exit)
    }
}

extension AudienceView {
    func startDisplay() {
        videoView.joinLiveStream(roomId: roomId) { [weak self] roomInfo in
            guard let self = self, let roomInfo = roomInfo else { return }
            manager.updateRoomState(roomInfo: roomInfo)
            manager.updateOwnerUserInfo()
            manager.update(liveStatus: .playing)
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(error: code, message: message)
            self.manager.toastSubject.send(error.localizedMessage)
        }
    }
}

extension AudienceView: VideoViewDelegate {
    func createCoGuestView(userInfo: TUIUserInfo) -> UIView? {
        return CoGuestView(userInfo: userInfo, manager: manager)
    }
    
    func updateCoGuestView(userInfo: TUIUserInfo, coGuestView: UIView) {
        
    }
    
    func createCoHostView(connectionUser: TUIConnectionUser) -> UIView? {
        return CoHostView(connectionUser: connectionUser, manager: manager)
    }
    
    func updateCoHostView(connectionUser: TUIConnectionUser, coHostView: UIView) {
        
    }
}

extension AudienceView: WaitingCoGuestViewDelegate {
    func waitingCoGuestView() -> UIView? {
        return WaitLinkMicAnimationView()
    }
}

extension String {
    fileprivate static let enterRoomFailedMessageText = localized("live.alert.enterRoom.failed.message.xxx")
    
    fileprivate static let balanceInsufficientText =
    localized("live.balanceInsufficient")
}
