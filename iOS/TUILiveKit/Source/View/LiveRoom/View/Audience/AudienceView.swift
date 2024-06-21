//
//  AudienceView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/10/19.
//

import Foundation
import RTCCommon
import Combine

class AudienceView: RTCBaseView {
    @Injected var store: LiveStore
    let roomId: String
    // MARK: - property: publisher
    lazy var  liveStatusPublisher = store.select(ViewSelectors.getLiveStatus)
    // MARK: - property: view
    let videoView: MatrixVideoRenderView = MatrixVideoRenderView(frame: .zero)
    let livingView: AudienceLivingView = AudienceLivingView(frame: .zero)
    lazy var dashboardView: AudienceEndView = {
        let roomOwner = store.selectCurrent(RoomSelectors.getRoomOwnerInfo)
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        let view = AudienceEndView(roomId: roomId, avatarUrl: roomOwner.avatarUrl,userName: roomOwner.name)
        view.isHidden = true
        return view
    }()
    
    lazy var giftPanelView: TUIGiftListView = {
        let view = TUIGiftListView(groupId: store.selectCurrent(RoomSelectors.getRoomId))
        view.delegate = self
        giftCloudServer.queryGiftInfoList { error, giftList in
            if error == .noError {
                view.setGiftList(giftList)
            }
        }
        view.setBalance(500)
        return view
    }()
    
    // MARK: - private property
    private var cancellableSet: Set<AnyCancellable> = []
    private let giftCloudServer: IGiftCloudServer = GiftCloudServer()
    
    init(roomId: String) {
        self.roomId = roomId
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    deinit {
        debugPrint("deinit \(self)")
    }
    
    
    override func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(videoView)
        addSubview(livingView)
        addSubview(dashboardView)
    }
    
    override func activateConstraints() {
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
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
        store.dispatch(action: UserActions.getSelfInfo())
        // prepare room state.
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        var state = RoomState()
        state.roomId = roomId
        store.dispatch(action: RoomActions.initializeRoomState(payload: state))
        startDisplay()
    }
}

extension AudienceView {
    private func subscribeCustomEvent() {
        store.userActionSubject
            .receive(on: DispatchQueue.main)
            .filter({
                $0.id == UserResponseActions.like.id
            })
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.giftPanelView.sendLike()
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeRoomState() {
        liveStatusPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                    case .none,.previewing:
                        // TODO: - mute all?
                        break
                    case .finished:
                        dashboardView.update(avatarUrl: self.store.roomState.ownerInfo.avatarUrl,
                                             userName: self.store.roomState.ownerInfo.name)
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
        let actions: [Action] = [
            RoomActions.fetchRoomInfo(),
            SeatActions.fetchSeatList(),
            UserActions.fetchUserList(),
            RoomActions.fetchRoomOwnerInfo(),
            UserActions.checkFollowType(payload: store.selectCurrent(RoomSelectors.roomOwnerId)),
        ]
        actions.forEach { store.dispatch(action: $0) }
    }
}

extension AudienceView: RouterViewProvider {
    func getRouteView(route: Route) -> UIView? {
        if route == .giftView {
            return giftPanelView
        } else {
            return nil
        }
    }
}

extension AudienceView: TUIGiftListViewDelegate {
    func onRecharge(giftListView view: TUIGiftListView) {
        giftCloudServer.rechargeBalance { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.setBalance(balance)
            } else {
                let toastInfo = ToastInfo(message: .balanceInsufficientText)
                store.dispatch(action: ViewActions.toastEvent(payload: toastInfo))
            }
        }
    }
    
    func onSendGift(giftListView view: TUIGiftListView, giftModel: TUIGift, giftCount: Int) {
        let anchorInfo = store.selectCurrent(RoomSelectors.getRoomOwnerInfo)
        let receiver = TUIGiftUser()
        receiver.userId = anchorInfo.userId
        receiver.userName = anchorInfo.name
        receiver.avatarUrl = anchorInfo.avatarUrl
        receiver.level = "0"
        
        let selfInfo = store.selectCurrent(UserSelectors.getSelfInfo)
        giftCloudServer.sendGift(sender: selfInfo.userId,
                                 receiver: receiver.userId,
                                 giftModel: giftModel,
                                 giftCount: giftCount) { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.sendGift(giftModel: giftModel, giftCount: giftCount, receiver: receiver)
                view.setBalance(balance)
            } else {
                let toastInfo = ToastInfo(message: .balanceInsufficientText)
                store.dispatch(action: ViewActions.toastEvent(payload: toastInfo))
            }
        }
    }
}

extension AudienceView {
     func startDisplay() {
         let param = generateActionParamTuple(param: roomId, actions: [])
         DataReporter.componentType = .liveRoom
         store.dispatch(action: RoomActions.join(payload: param))
    }
    
    func stopDisplay() {
        
    }
    
    func endDisplay() {
        
    }
}

extension String {
    fileprivate static let enterRoomFailedMessageText = localized("live.alert.enterRoom.failed.message.xxx")
    
    fileprivate static let balanceInsufficientText =
            localized("live.balanceInsufficient")
}
