//
//  File.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/14.
//

import Foundation
import TUICore
import RTCRoomEngine
import Combine


class AnchorView: UIView {
    private let roomId:String
    private let store: LiveStoreProvider
    private let routerStore: RouterStore
    private lazy var liveStatusPublisher = store.select(ViewSelectors.getLiveStatus)
    private lazy var getReceivedConnectionPublisher = store.select(ConnectionSelectors.getReceivedConnectionRequest)
    private var cancellableSet = Set<AnyCancellable>()
    var startLiveBlock:(()->Void)?
    
    private lazy var videoView: AnchorVideoView = {
        AnchorVideoView(store: store)
    }()
    
    private lazy var prepareView: AnchorPrepareView = {
        let view = AnchorPrepareView(store: store, routerStore: routerStore)
        view.delegate = self
        return view
    }()
    
    private lazy var livingView: AnchorLivingView = {
        let view = AnchorLivingView(roomId: roomId, routerStore: routerStore)
        view.alpha = 0
        return view
    }()
    
    lazy var beautyPanelView: UIView = {
        let roomEngine = store.roomEngine
        let view = TUIBeautyPanel(store: store, routerStore: routerStore)
        view.frame = CGRect(x: -100, y: -100, width: 0, height: 0)
        return view
    }()
    
    private weak var alertPanel: AlertPanel?

    init(roomId: String, routerStore: RouterStore) {
        self.roomId = roomId
        self.store = LiveStoreFactory.getStore(roomId: roomId)
        self.routerStore = routerStore
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .black
        isViewReady = true
        initData()
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    
    func updateRootViewOrientation(isPortrait: Bool) {
        prepareView.updateRootViewOrientation(isPortrait: isPortrait)
        livingView.updateRootViewOrientation(isPortrait: isPortrait)
    }
    
    func initData() {
        store.dispatch(action: RoomActions.updateRoomCategory(payload: LiveStreamCategory.chat))
        store.dispatch(action: RoomActions.updateRoomMode(payload: LiveStreamPrivacyStatus.public))
        store.dispatch(action: BeautyActions.setRuddyLevel(payload: store.beautyState.ruddyLevel))
        store.dispatch(action: BeautyActions.setWhitenessLevel(payload: store.beautyState.whitenessLevel))
        store.dispatch(action: BeautyActions.setSmoothLevel(payload: store.beautyState.smoothLevel))
        store.dispatch(action: ViewActions.updateLiveStatus(payload: .previewing))
    }
}



extension AnchorView {
    
    private func constructViewHierarchy() {
        addSubview(videoView)
        addSubview(prepareView)
        addSubview(livingView)
        addSubview(beautyPanelView)
    }
    
    private func activateConstraints() {
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        
        prepareView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        
        livingView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
    }
    
    private func bindInteraction() {
        store.dispatch(action: MediaActions.operateCamera(payload: true))
        store.dispatch(action: UserActions.getSelfInfo())
        subscribeAlertState()
        subscribeConnectionState()
    }
    
}

// MARK: Action

extension AnchorView {
    private func showEndView() {
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        if store.selectCurrent(UserSelectors.isOwner) {
            let roomState = store.selectCurrent(RoomSelectors.getRoomState)
            let giftIncome = store.selectCurrent(RoomSelectors.getGiftIncome)
            let giftPeopleCount = store.selectCurrent(RoomSelectors.getGiftPeopleSet).count
            let audienceCount = store.selectCurrent(UserSelectors.getUserList).count
            let liveDataModel = LiveDataModel(roomId: roomId,
                                              liveDuration: abs(Int(Date().timeIntervalSince1970 - Double(roomState.createTime / 1_000))),
                                              audienceCount: audienceCount == 0 ? 0 : audienceCount - 1,
                                              messageCount: livingView.getBarrageCount(),
                                              giftIncome: giftIncome,
                                              giftPeopleCount: giftPeopleCount,
                                              likeCount: livingView.getLikeCount())
            let anchorEndView = AnchorEndView(liveDataModel: liveDataModel, routerStore: routerStore)
            addSubview(anchorEndView)
            anchorEndView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        } else {
            let roomOwner = store.selectCurrent(RoomSelectors.getRoomOwnerInfo)
            let audienceEndView = AudienceEndView(roomId: roomId, avatarUrl: roomOwner.avatarUrl, userName: roomOwner.name)
            addSubview(audienceEndView)
            audienceEndView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        }
    }
    
    private func startLiving() {
        self.prepareView.alpha = 0
        UIView.animate(withDuration: 0.5) { [weak self] in
            guard let self = self else { return }
            self.livingView.alpha = 1
        } completion: { [weak self] _ in
            guard let self = self else { return }
            self.prepareView.removeFromSuperview()
        }
        startLiveBlock?()
    }
    
    private func createRoom() {
        store.dispatch(action: RoomActions.updateRoomId(payload: roomId))
        startLiving()
        subscribeLiveStatus()
        
        let roomInfo = TUIRoomInfo()
        roomInfo.roomId = store.selectCurrent(RoomSelectors.getRoomId)
        roomInfo.name = store.selectCurrent(RoomSelectors.getRoomName)
        roomInfo.isSeatEnabled = true
        roomInfo.roomType = .live
        roomInfo.seatMode = store.roomState.seatMode
        roomInfo.maxSeatCount = store.roomState.maxSeatCount
        let config = generateActionParamTuple(param: roomInfo, actions: [])
        DataReporter.componentType = .liveRoom
        store.dispatch(action: RoomActions.start(payload: config))
    }
    
    private func subscribeLiveStatus() {
        liveStatusPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                guard let self = self else { return }
                switch status {
                case .pushing:
                    self.store.dispatch(action: SeatActions.takeSeat(payload: nil))
                default:
                    break
                }
            }
            .store(in: &cancellableSet)
    }

    private func subscribeConnectionState() {
        getReceivedConnectionPublisher.receive(on: RunLoop.main)
            .sink { [weak self] connectionRequest in
                guard let self = self else { return }
                if let request = connectionRequest {
                    let alertInfo = AlertInfo(description: String.localizedReplace(.connectionInviteText, replace: "\(request.userName)"),
                                              imagePath: request.avatarUrl,
                                              cancelButtonInfo: (String.rejectText, .g3),
                                              defaultButtonInfo: (String.acceptText, .b1)) { [weak self] alertPanel in
                        guard let self = self else { return }
                        self.store.dispatch(action: ConnectionActions.reject(payload: request.roomId))
                    } defaultClosure: { [weak self] alertPanel in
                        guard let self = self else { return }
                        self.store.dispatch(action: ConnectionActions.accept(payload: request.roomId))
                    }
                    self.store.dispatch(action: ViewActions.alertEvent(payload: alertInfo))
                } else {
                    self.alertPanel?.dismiss()
                }
            }
            .store(in: &cancellableSet)
    }

    private func subscribeAlertState() {
        store.alertSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] alertInfo in
                guard let self = self else { return }
                let alertPanel = AlertPanel(alertInfo: alertInfo)
                alertPanel.show()
                self.alertPanel = alertPanel
            }
            .store(in: &cancellableSet)
    }
}

extension AnchorView : AnchorPrepareViewDelegate {
    
    func prepareView(_ view: AnchorPrepareView, didClickStart button: UIButton) {
        createRoom()
    }
    
}

private extension String {
    static var enterRoomFailedTitleText = localized("live.alert.enterRoom.failed.title")
    static var enterRoomFailedMessageText = localized("live.alert.enterRoom.failed.message.xxx")
    static var confirmText = localized("live.alert.confirm")
    static var roomNameEmptyToast = localized("live.anchor.room.name.empty.toast")
    static var operateFailedText = localized("live.operation.fail.xxx")
    
    static let connectionInviteText = localized("live.connection.invite.desc.xxx")
    static let rejectText = localized("live.alert.refuse")
    static let acceptText = localized("live.anchor.link.agree.title")
}
