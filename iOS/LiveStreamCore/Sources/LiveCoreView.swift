//
//  LiveCoreView.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import SnapKit
import Combine
import RTCCommon
import RTCRoomEngine
import TUICore

@objcMembers public class LiveCoreView: UIView {
    public weak var videoViewDelegate: VideoViewDelegate? {
        didSet {
            LCDataReporter.reportEventData(event: .methodCallLiveStreamSetVideoViewDelegate)
        }
    }
    public weak var waitingCoGuestViewDelegate: WaitingCoGuestViewDelegate?
    
    private let videoLiveManager: LiveStreamManager = LiveStreamManager()
    private var layoutConfig: LayoutConfig?
    private var cancellableSet: Set<AnyCancellable> = []
    private var isViewReady = false
    
    private var coGuestUserList: [TUISeatInfo] = []
    private var coHostUserList: [TUIConnectionUser] = []
    private var waitingCoGuestView: UIView?
    private var videoViewModelMap: [String: VideoViewModel] = [:]
    
    public init() {
        super.init(frame: .zero)
        LCDataReporter.reportEventData(event: .panelShowLiveCoreView)
        videoLiveManager.context.delegate = self
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        LCDataReporter.reportEventData(event: .panelHideLiveCoreView)
        debugPrint("deinit:\(self)")
    }
    
    override public func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        initLayout()
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
        isViewReady = true
    }
    
    private lazy var liveStreamContainerView: LiveStreamViewContainer = {
        let view = LiveStreamViewContainer()
        view.layoutConfig = layoutConfig
        return view
    }()
}

// MARK: - Public API
public extension LiveCoreView {
    func startCamera(useFrontCamera: Bool, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        videoLiveManager.startLocalCamera(useFrontCamera: useFrontCamera, onSuccess: onSuccess, onError: onError)
    }
    
    func startMicrophone(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        videoLiveManager.startMicrophone(onSuccess: onSuccess, onError: onError)
    }
    
    func muteMicrophone(mute: Bool) {
        videoLiveManager.muteMicrophone(mute: mute)
    }
    
    func stopCamera() {
        videoLiveManager.stopCamera()
    }
    
    func stopMicrophone() {
        videoLiveManager.stopMicrophone()
    }
    
    func startLiveStream(roomInfo: TUIRoomInfo, onSuccess: @escaping TUIRoomInfoBlock, onError: @escaping TUIErrorBlock) {
        videoLiveManager.startLive(roomInfo: roomInfo, onSuccess: onSuccess, onError: onError)
    }
    
    func stopLiveStream(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        videoLiveManager.stopLive(onSuccess: onSuccess, onError: onError)
    }
    
    func joinLiveStream(roomId: String, onSuccess: @escaping TUIRoomInfoBlock, onError: @escaping TUIErrorBlock) {
        videoLiveManager.joinLive(roomId: roomId, onSuccess: onSuccess, onError: onError)
    }
    
    func leaveLiveStream(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        videoLiveManager.leaveLive(onSuccess: onSuccess, onError: onError)
    }
    
    func requestIntraRoomConnection(userId: String, timeOut: Int, openCamera: Bool,
                                    onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        requestIntraConnection(userId: userId, timeOut: timeOut, openCamera: openCamera,
                               onSuccess: onSuccess, onError: onError)
    }
    
    func cancelIntraRoomConnection(userId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        cancelIntraConnection(userId: userId, onSuccess: onSuccess, onError: onError)
    }
    
    func respondIntraRoomConnection(userId: String, isAccepted: Bool,
                                    onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        respondIntraConnection(userId: userId, isAccepted: isAccepted, onSuccess: onSuccess, onError: onError)
    }
    
    func disconnectUser(userId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        if checkIntraRoomConnection(userId: userId, onError: onError) {
            return
        }
        videoLiveManager.disconnectByAdamin(userId: userId, onSuccess: onSuccess, onError: onError)
    }
    
    func terminateIntraRoomConnection() {
        videoLiveManager.disconnectBySelf()
    }
    
    func requestCrossRoomConnection(roomId: String, timeOut: Int,
                                    onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        if checkCrossRoomConnection(roomId: roomId, onError: onError) {
            return
        }
        videoLiveManager.requestConnection(roomId: roomId, timeOut: timeOut, onSuccess: onSuccess, onError: onError)
    }
    
    func cancelCrossRoomConnection(roomId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        videoLiveManager.cancelRequest(roomId: roomId, onSuccess: onSuccess, onError: onError)
    }
    
    func respondToCrossRoomConnection(roomId: String, isAccepted: Bool,
                                      onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        videoLiveManager.respondToCrossRoomConnection(roomId: roomId, isAccepted: isAccepted,
                                                      onSuccess: onSuccess, onError: onError)
    }
    
    func terminateCrossRoomConnection() {
        videoLiveManager.disconnect()
    }
    
    func registerConnectionObserver(observer: ConnectionObserver) {
        videoLiveManager.addObserver(observer)
    }
    
    func unregisterConnectionObserver(observer: ConnectionObserver) {
        videoLiveManager.removerObserver(observer)
    }
    
    func setLayoutMode(layoutMode: LayoutMode, layoutJson: String? = nil) {
        videoLiveManager.setLayoutMode(layoutMode: layoutMode, layoutJson: layoutJson)
    }
    
    func getMediaManager() -> MediaManager {
        return videoLiveManager.context.mediaManager
    }
}

// MARK: - Private
extension LiveCoreView {
    private func initLayout() {
        videoLiveManager.setLayoutMode(layoutMode: .gridLayout)
    }
    
    private func constructViewHierarchy() {
        addSubview(liveStreamContainerView)
    }
    
    private func activateConstraints() {
        liveStreamContainerView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func subscribeState() {
        videoLiveManager.subscribeCoGuestState(StateSelector(keyPath: \CoGuestState.connectedUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] coGuestUsers in
                guard let self = self else { return }
                self.onCoGuestUserListChanged(coGuestUsers: coGuestUsers)
            }
            .store(in: &cancellableSet)
        
        videoLiveManager.subscribeCoGuestState(StateSelector(keyPath: \CoGuestState.coGuestStatus))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] coGuestStatus in
                guard let self = self else { return }
                guard let waitingCoGuestViewDelegate = self.waitingCoGuestViewDelegate else { return }
                switch coGuestStatus {
                case .applying:
                    waitingCoGuestView = waitingCoGuestView ?? waitingCoGuestViewDelegate.waitingCoGuestView()
                    self.liveStreamContainerView.addView(waitingCoGuestView ?? UIView())
                default:
                    guard let waitingCoGuestView = waitingCoGuestView else { return }
                    self.liveStreamContainerView.removeView(waitingCoGuestView)
                    self.waitingCoGuestView = nil
                }
            }
            .store(in: &cancellableSet)
        
        videoLiveManager.subscribeCoHostState(StateSelector(keyPath: \CoHostState.connectedUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] coHostUsers in
                guard let self = self else { return }
                self.onCoHostUserListChanged(coHostUsers: coHostUsers)
            }
            .store(in: &cancellableSet)
        
        videoLiveManager.subscribeRoomState(StateSelector(keyPath: \RoomState.liveStatus))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] status in
                guard let self = self else { return }
                if status == .none {
                    self.liveStreamContainerView.removeAllViews()
                }
            }
            .store(in: &cancellableSet)
        
        videoLiveManager.subscribeViewState(StateSelector(keyPath: \ViewState.layoutMode))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] layoutConfig in
                guard let self = self, let layoutConfig = videoLiveManager.viewState.layoutConfig else { return }
                self.liveStreamContainerView.setLayoutConfig(layoutConfig: layoutConfig)
            }
            .store(in: &cancellableSet)
        
        videoLiveManager.subscribeMediaState(StateSelector(keyPath: \MediaState.isCameraOpened))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isCameraOpened in
                guard let self = self else { return }
                isCameraOpened ? addLocalVideoView() : removeLocalVideoView()
            }
            .store(in: &cancellableSet)
    }
    
    private func addLocalVideoView() {
        let localVideoView = videoLiveManager.getLocalLiveView()
        videoLiveManager.setLocalVideoView(view: localVideoView.videoView)
        if !liveStreamContainerView.containsView(localVideoView) {
            if let videoViewDelegate = videoViewDelegate {
                let selfInfo = videoLiveManager.userState.selfInfo
                let selfUserId = selfInfo.userId
                
                let coGuestWidgetsView = videoViewDelegate.createCoGuestView(userInfo: videoLiveManager.userState.selfInfo)
                localVideoView.setCoGuestView(coGuestWidgetsView)
                let videoViewModel = VideoViewModel()
                videoViewModel.coGuestUser = selfInfo
                videoViewModel.userView = coGuestWidgetsView
                videoViewModelMap[selfUserId] = videoViewModel
                
                let hasVideo = videoLiveManager.userState.hasVideoStreamUserList.contains(selfUserId) ||
                               videoLiveManager.mediaState.isCameraOpened
                let hasAudio = videoLiveManager.userState.hasAudioStreamUserList.contains(selfUserId) ||
                               !videoLiveManager.mediaState.isMicrophoneMuted
                let coHostUser = LiveStreamConvert.convertToCoHostUser(userInfo: selfInfo, roomId: videoLiveManager.roomState.roomId, hasVideoStream: hasVideo, hasAudioStream: hasAudio)
                let coHostWidgetsView = videoViewDelegate.createCoHostView(coHostUser: coHostUser)
                localVideoView.setCoHostView(coHostWidgetsView)
            }
            liveStreamContainerView.addView(localVideoView)
        }
    }
    
    private func removeLocalVideoView() {
        let localVideoView = videoLiveManager.getLocalLiveView()
        videoLiveManager.setLocalVideoView(view: nil)
        if liveStreamContainerView.containsView(localVideoView) {
            liveStreamContainerView.removeView(localVideoView)
            videoLiveManager.clearLocalLiveView()
        }
        videoViewModelMap.removeValue(forKey: videoLiveManager.userState.selfInfo.userId)
    }
    
    private func addCoGuestLiveView(userInfo: TUIUserInfo) {
        if userInfo.userId == videoLiveManager.userState.selfInfo.userId {
            return
        }
        let liveView = videoLiveManager.getRemoteLiveViewByUserId(userId: userInfo.userId)
        videoLiveManager.setRemoteVideoView(userId: userInfo.userId, streamType: .cameraStream, view: liveView.videoView)
        if !liveStreamContainerView.containsView(liveView) {
            if let videoViewDelegate = videoViewDelegate {
                let coGuestWidgetsView = videoViewDelegate.createCoGuestView(userInfo: userInfo)
                liveView.setCoGuestView(coGuestWidgetsView)
                
                let videoViewModel = VideoViewModel()
                videoViewModel.coGuestUser = userInfo
                videoViewModel.userView = coGuestWidgetsView
                videoViewModelMap[userInfo.userId] = videoViewModel
            }
            liveStreamContainerView.addView(liveView)
        }
    }
    
    private func removeCoGuestLiveView(userInfo: TUIUserInfo) {
        if userInfo.userId == videoLiveManager.userState.selfInfo.userId {
            return
        }
        let liveView = videoLiveManager.getRemoteLiveViewByUserId(userId: userInfo.userId)
        videoLiveManager.removeRemoteView(userId: userInfo.userId)
        liveStreamContainerView.removeView(liveView)
        videoViewModelMap.removeValue(forKey: userInfo.userId)
    }
    
    private func addCoHostLiveView(user: TUIConnectionUser) {
        if user.userId == videoLiveManager.userState.selfInfo.userId {
            return
        }
        let liveView = videoLiveManager.getRemoteLiveViewByUserId(userId: user.userId)
        videoLiveManager.setRemoteVideoView(userId: user.userId, streamType: .cameraStream, view: liveView.videoView)
        if !liveStreamContainerView.containsView(liveView) {
            if let videoViewDelegate = videoViewDelegate {
                let hasVideo = videoLiveManager.userState.hasVideoStreamUserList.contains(user.userId)
                let hasAudio = videoLiveManager.userState.hasAudioStreamUserList.contains(user.userId)
                let coHostUser = LiveStreamConvert.convertToCoHostUser(connectionUser: user, hasVideoStream: hasVideo, hasAudioStream: hasAudio)
                let coHostWidgetsView = videoViewDelegate.createCoHostView(coHostUser: coHostUser)
                liveView.setCoHostView(coHostWidgetsView)
                
                let videoViewModel = VideoViewModel()
                videoViewModel.coHostUser = coHostUser
                videoViewModel.userView = coHostWidgetsView
                videoViewModelMap[user.userId] = videoViewModel
            }
            liveStreamContainerView.addView(liveView)
        }
    }
    
    private func removeCoHostLiveView(user: TUIConnectionUser) {
        if user.userId == videoLiveManager.userState.selfInfo.userId ||
            user.userId == videoLiveManager.roomState.ownerInfo.userId {
            return
        }
        let liveView = videoLiveManager.getRemoteLiveViewByUserId(userId: user.userId)
        videoLiveManager.removeRemoteView(userId: user.userId)
        liveStreamContainerView.removeView(liveView)
        videoViewModelMap.removeValue(forKey: user.userId)
    }
    
    private func requestIntraConnection(userId: String, timeOut: Int, openCamera: Bool,
                                        onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        videoLiveManager.enableAutoOpenCameraOnSeated(enable: openCamera)
        if checkRequestIntraRoomConnection(userId: userId, onError: onError) {
            return
        }
        if userId.isEmpty || userId == videoLiveManager.roomState.ownerInfo.userId {
            applyToConnection(timeOut: timeOut, onSuccess: onSuccess, onError: onError)
        } else {
            videoLiveManager.inviteGuestToConnection(userId: userId, onSuccess: onSuccess, onError: onError)
        }
    }
    
    private func applyToConnection(timeOut: Int, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        Task {
            do {
                let result = try await videoLiveManager.applyToConnection(timeOut: timeOut) { [weak self] in
                    self?.videoLiveManager.asyncRunMainThread(onSuccess)
                }
                switch result {
                case .accepted(userId: _):
                    if !videoLiveManager.coGuestState.openCameraOnCoGuest {
                        addLocalVideoView()
                    } else {
                        startCamera(useFrontCamera: true) {
                        } onError: { _, _ in
                        }
                    }
                    startMicrophone() {
                    } onError: { _, _ in
                    }
                default: break
                }
            } catch let LiveStreamCoreError.error(code, message) {
                videoLiveManager.asyncRunMainThread(onError, code, message)
            }
        }
    }
    
    private func cancelIntraConnection(userId: String,
                                       onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        if checkIntraRoomConnection(userId: userId, onError: onError) {
            return
        }
        if userId.isEmpty || userId == videoLiveManager.roomState.ownerInfo.userId {
            videoLiveManager.cancelGuestApplication(onSuccess: onSuccess, onError: onError)
        } else {
            videoLiveManager.cancelInviteApplication(userId: userId, onSuccess: onSuccess, onError: onError)
        }
    }
    
    private func respondIntraConnection(userId: String, isAccepted: Bool,
                                         onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        if checkIntraRoomConnection(userId: userId, onError: onError) {
            return
        }
        if userId.isEmpty || userId == videoLiveManager.roomState.ownerInfo.userId {
            videoLiveManager.respondGuestInvitation(isAgree: isAccepted, onSuccess: onSuccess, onError: onError)
        } else {
            videoLiveManager.respondGuestApplication(userId: userId, isAccepted: isAccepted,
                                                     onSuccess: onSuccess, onError: onError)
        }
    }
    
    private func checkRequestIntraRoomConnection(userId: String, onError: @escaping TUIErrorBlock) -> Bool {
        if checkIntraRoomConnection(userId: userId, onError: onError) {
            return true
        }
        if !videoLiveManager.coHostState.connectedUserList.isEmpty ||
            !videoLiveManager.coHostState.sentConnectionRequestList.isEmpty {
            onError(.roomConnectedInOther, "When connecting across rooms, viewers in the room are not allowed to connect")
            return true
        }
        return false
    }
    
    private func checkIntraRoomConnection(userId: String, onError: @escaping TUIErrorBlock) -> Bool {
        if videoLiveManager.roomState.liveStatus == .none {
            onError(.operationInvalidBeforeEnterRoom,
                    TUIError.operationInvalidBeforeEnterRoom.lcDescription)
            return true
        }
        if !videoLiveManager.isCoGuestEnable() {
            onError(.failed, "The audience connection function is disabled in the current room")
            return true
        }
        if videoLiveManager.userState.selfInfo.userRole == .roomOwner && userId.isEmpty {
            onError(.failed, "userId is empty")
            return true
        }
        return false
    }
    
    private func checkCrossRoomConnection(roomId: String, onError: @escaping TUIErrorBlock) -> Bool {
        if videoLiveManager.roomState.liveStatus == .none {
            onError(.operationInvalidBeforeEnterRoom,
                    TUIError.operationInvalidBeforeEnterRoom.lcDescription)
            return true
        }
        if !videoLiveManager.isCoHostEnable() {
            onError(.failed, "Room connection function is disabled")
            return true
        }
        if videoLiveManager.coGuestState.connectedUserList.count > 1 ||
            !videoLiveManager.coGuestState.connectionRequestList.isEmpty ||
            videoLiveManager.coHostState.receivedConnectionRequest != nil {
            onError(.alreadyInSeat, "Cross-room connections are not allowed when there are viewers connected in the room")
            return true
        }
        if roomId.isEmpty {
            onError(.invalidParameter, "roomId is empty")
            return true
        }
        return false
    }
    
    private func onCoGuestUserListChanged(coGuestUsers: [TUISeatInfo]) {
        let (addedUsers, removedUsers) = calculateCoGuestUserChanges(newList: coGuestUsers)
        for seatInfo in addedUsers {
            coGuestUserList.append(seatInfo)
            addCoGuestLiveView(userInfo: LiveStreamConvert.convertToUserInfo(seatInfo: seatInfo))
            if seatInfo.userId == videoLiveManager.userState.selfInfo.userId {
                addLocalVideoView()
            }
        }
        for seatInfo in removedUsers {
            coGuestUserList.removeAll { $0.userId == seatInfo.userId }
            removeCoGuestLiveView(userInfo: LiveStreamConvert.convertToUserInfo(seatInfo: seatInfo))
            if seatInfo.userId == videoLiveManager.userState.selfInfo.userId {
                removeLocalVideoView()
            }
        }
    }
    
    private func onCoHostUserListChanged(coHostUsers: [TUIConnectionUser]) {
        for user in coHostUsers {
            if videoLiveManager.roomState.ownerInfo.userId == user.userId {
                addCoHostLiveView(user: user)
                break
            }
        }
        let (addedUsers, removedUsers) = calculateCoHostUserChanges(newList: coHostUsers)
        for user in addedUsers {
            coHostUserList.append(user)
            addCoHostLiveView(user: user)
        }
        for user in removedUsers {
            coHostUserList.removeAll { $0.userId == user.userId }
            removeCoHostLiveView(user: user)
        }
    }
    
    private func calculateCoGuestUserChanges(newList: [TUISeatInfo]) -> (added: [TUISeatInfo], removed: [TUISeatInfo]) {
        let newSet = Set(newList.map { $0.userId })
        let oldSet = Set(coGuestUserList.map { $0.userId })

        let addedUsers = newList.filter { !oldSet.contains($0.userId) }
        let removedUsers = coGuestUserList.filter { !newSet.contains($0.userId) }

        return (addedUsers, removedUsers)
    }
    
    private func calculateCoHostUserChanges(newList: [TUIConnectionUser]) -> (added: [TUIConnectionUser], removed: [TUIConnectionUser]) {
        let newSet = Set(newList.map { $0.userId })
        let oldSet = Set(coHostUserList.map { $0.userId })
        
        let addedUsers = newList.filter { !oldSet.contains($0.userId) }
        let removedUsers = coHostUserList.filter { !newSet.contains($0.userId) }
        
        return (addedUsers, removedUsers)
    }
}

// MARK: - UpdateUserInfoDelegate
extension LiveCoreView: UpdateUserInfoDelegate {
    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason) {
        guard let videoViewDelegate = videoViewDelegate,
              let videoViewModel = videoViewModelMap[userId],
              let userView = videoViewModel.userView else { return }
        let modifyFlags: UserInfoModifyFlag = [.hasAudioStream]
        if let coGuestUser = videoViewModel.coGuestUser {
            coGuestUser.hasAudioStream = hasAudio
            videoViewDelegate.updateCoGuestView(userInfo: coGuestUser, modifyFlag: modifyFlags, coGuestView: userView)
        } else if let coHostUser = videoViewModel.coHostUser {
            coHostUser.hasAudioStream = hasAudio
            videoViewDelegate.updateCoHostView(coHostUser: coHostUser, modifyFlag: modifyFlags, coHostView: userView)
        }
    }
    
    func onUserVideoStateChanged(userId: String, hasVideo: Bool, reason: TUIChangeReason) {
        guard let videoViewDelegate = videoViewDelegate,
              let videoViewModel = videoViewModelMap[userId],
              let userView = videoViewModel.userView else { return }
        let modifyFlags: UserInfoModifyFlag = [.hasVideoStream]
        if let coGuestUser = videoViewModel.coGuestUser {
            coGuestUser.hasVideoStream = hasVideo
            videoViewDelegate.updateCoGuestView(userInfo: coGuestUser, modifyFlag: modifyFlags, coGuestView: userView)
        } else if let coHostUser = videoViewModel.coHostUser {
            coHostUser.hasAudioStream = hasVideo
            videoViewDelegate.updateCoHostView(coHostUser: coHostUser, modifyFlag: modifyFlags, coHostView: userView)
        }
    }
    
    func onUserInfoChanged(userInfo: TUIUserInfo, modifyFlag: TUIUserInfoModifyFlag) {
        guard let videoViewDelegate = videoViewDelegate,
              let videoViewModel = videoViewModelMap[userInfo.userId],
              let userView = videoViewModel.userView else { return }
        let modifyFlags: UserInfoModifyFlag = LiveStreamConvert.convertToUserInfoModifyFlag(flag: modifyFlag)
        if var coGuestUser = videoViewModel.coGuestUser {
            coGuestUser = userInfo
            videoViewDelegate.updateCoGuestView(userInfo: coGuestUser, modifyFlag: modifyFlags, coGuestView: userView)
        }
    }
}

class VideoViewModel {
    var coHostUser: CoHostUser?
    var coGuestUser: TUIUserInfo?
    var userView: UIView?
}
