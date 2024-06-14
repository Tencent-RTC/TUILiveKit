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
    @Injected var store: LiveStore
    private lazy var liveStatusPublisher = store.select(ViewSelectors.getLiveStatus)
    private var cancellableSet = Set<AnyCancellable>()
    var startLiveBlock:(()->Void)?
    
    private lazy var videoView: AnchorVideoView = {
        AnchorVideoView()
    }()
    
    private lazy var prepareView: AnchorPrepareView = {
        let view = AnchorPrepareView(frame: .zero)
        view.delegate = self
        return view
    }()
    
    private lazy var livingView: AnchorLivingView = {
        let view = AnchorLivingView(roomId: roomId)
        view.alpha = 0
        return view
    }()
    
    var musicPanelView:MusicPanelView {
        return livingView.musicPanelView
    }
    
    init(roomId: String) {
        self.roomId = roomId
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
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
            let audienceCount = store.selectCurrent(UserSelectors.getAudienceUserList).count
            let liveDataModel = LiveDataModel(roomId: roomId,
                                              liveDuration: abs(Int(Date().timeIntervalSince1970 - Double(roomState.createTime / 1_000))),
                                              audienceCount: audienceCount,
                                              messageCount: livingView.getBarrageCount(),
                                              giftIncome: giftIncome,
                                              giftPeopleCount: giftPeopleCount,
                                              likeCount: livingView.getLikeCount())
            let anchorEndView = AnchorEndView(liveDataModel: liveDataModel)
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
        startLiving()
        subscribeLiveStatus()
        let roomInfo = TUIRoomInfo()
        roomInfo.roomId = roomId
        roomInfo.name = store.roomState.roomName
        roomInfo.isSeatEnabled = true
        roomInfo.seatMode = store.roomState.seatMode
        roomInfo.maxSeatCount = store.roomState.maxSeatCount
        roomInfo.roomType = .live
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
}

extension AnchorView : AnchorPrepareViewDelegate {
    
    func prepareView(_ view: AnchorPrepareView, didClickStart button: UIButton) {
        createRoom()
    }
    
}

private extension String {
    static var enterRoomFailedTitleText = {
        localized("live.alert.enterRoom.failed.title")
    }()

    static var enterRoomFailedMessageText = {
        localized("live.alert.enterRoom.failed.message.xxx")
    }()

    static var confirmText = {
        localized("live.alert.confirm")
    }()

    static var roomNameEmptyToast = {
        localized("live.anchor.room.name.empty.toast")
    }()
    
    static var operateFailedText: String {
        localized("live.operation.fail.xxx")
    }
}
