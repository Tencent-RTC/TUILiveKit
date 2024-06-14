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
    
    // MARK: - private property
    private var cancellableSet: Set<AnyCancellable> = []
    
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
        subscribeRoomState()
    }
}

extension AudienceView {
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

extension AudienceView {
     func startDisplay() {
         let param = generateActionParamTuple(param: roomId, actions: [])
         DataReporter.componentType = .liveRoom
         store.dispatch(action: RoomActions.join(payload: param))
        //        engineService.muteAllRemoteAudio(isMute: true)
    }
    
    func displayComplete() {
        //        engineService.muteAllRemoteAudio(isMute: false)
    }
    
    func endDisplay() {
        //        engineService.muteAllRemoteAudio(isMute: true)
    }
}

private extension String {
    static var enterRoomFailedMessageText = {
        localized("live.alert.enterRoom.failed.message.xxx")
    }()
}
