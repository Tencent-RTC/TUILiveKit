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
    
    // MARK: - public property.
    public weak var videoViewDelegate: VideoViewDelegate? {
        didSet {
            LCDataReporter.reportEventData(event: .methodCallLiveStreamSetVideoViewDelegate)
        }
    }
    public weak var waitingCoGuestViewDelegate: WaitingCoGuestViewDelegate?
    
    public var roomState: RoomState {
        manager.roomState
    }
    public var userState: UserState {
        manager.userState
    }
    public var mediaState: MediaState {
        manager.mediaState
    }
    
    public init() {
        super.init(frame: .zero)
        LCDataReporter.reportEventData(event: .panelShowLiveCoreView)
        manager.context.userManager.delegate = self
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
    
    // MARK: - private store property.
    private let manager: LiveStreamManager = LiveStreamManager()
    private let viewModel: LiveCoreViewModel = LiveCoreViewModel()
    private var cancellableSet: Set<AnyCancellable> = []
    private var isViewReady = false
    
    private var coGuestUserList: [TUISeatInfo] = []
    private var coHostUserList: [TUIConnectionUser] = []
    private var waitingCoGuestView: UIView?
    private var videoViewModelMap: [String: VideoViewModel] = [:]
    private var videoLayoutList: [ViewInfo] = []
    private var videoLayoutViewMap: [String: UIView] = [:]
    private var battleViewInfoMap: [String: BattleViewInfo] = [:]
    
    private lazy var liveStreamContainerView: LiveStreamViewContainer = {
        let view = LiveStreamViewContainer()
        return view
    }()
    
    private lazy var layoutContainerView: LiveStreamViewContainer = {
        let view = LiveStreamViewContainer()
        return view
    }()
}

// MARK: - Public API
extension LiveCoreView {
    
    public func startLiveStream(roomInfo: TUIRoomInfo, onSuccess: @escaping TUIRoomInfoBlock, onError: @escaping TUIErrorBlock) {
        manager.startLive(roomInfo: roomInfo, onSuccess: onSuccess, onError: onError)
    }
    
    public func stopLiveStream(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        terminateBattle(battleId: manager.battleState.battleId) {} onError: { _, _ in}
        manager.stopLive(onSuccess: onSuccess, onError: onError)
    }
    
    public func joinLiveStream(roomId: String, onSuccess: @escaping TUIRoomInfoBlock, onError: @escaping TUIErrorBlock) {
        manager.joinLive(roomId: roomId, onSuccess: onSuccess, onError: onError)
    }
    
    public func leaveLiveStream(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.leaveLive(onSuccess: onSuccess, onError: onError)
    }
    
    public func startCamera(useFrontCamera: Bool, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.startLocalCamera(useFrontCamera: useFrontCamera, onSuccess: onSuccess, onError: onError)
    }
    
    public func stopCamera() {
        manager.stopCamera()
    }
    
    public func startMicrophone(onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.startMicrophone(onSuccess: onSuccess, onError: onError)
    }
    
    public func muteMicrophone(mute: Bool) {
        manager.muteMicrophone(mute: mute)
    }
    
    public func stopMicrophone() {
        manager.stopMicrophone()
    }
    
    public func requestIntraRoomConnection(userId: String, timeOut: Int, openCamera: Bool,
                                    onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.requestIntraRoomConnection(userId: userId, timeOut: timeOut, openCamera: openCamera, onSuccess: onSuccess, onError: onError)
    }
    
    public func cancelIntraRoomConnection(userId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.cancelIntraRoomConnection(userId: userId, onSuccess: onSuccess, onError: onError)
    }
    
    public func respondIntraRoomConnection(userId: String, isAccepted: Bool,
                                    onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.respondIntraRoomConnection(userId: userId, isAccepted: isAccepted, onSuccess: onSuccess, onError: onError)
    }
    
    public func disconnectUser(userId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.disconnectByAdamin(userId: userId, onSuccess: onSuccess, onError: onError)
    }
    
    public func terminateIntraRoomConnection() {
        manager.disconnectBySelf()
    }
    
    public func requestCrossRoomConnection(roomId: String, timeOut: Int,
                                    onSuccess: @escaping ((TUIConnectionCode?) -> ()), onError: @escaping TUIErrorBlock) {
        manager.requestConnection(roomId: roomId, timeOut: timeOut, onSuccess: onSuccess, onError: onError)
    }
    
    public func cancelCrossRoomConnection(roomId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.cancelRequest(roomId: roomId, onSuccess: onSuccess, onError: onError)
    }
    
    public func respondToCrossRoomConnection(roomId: String, isAccepted: Bool,
                                      onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.respondToCrossRoomConnection(roomId: roomId, isAccepted: isAccepted,
                                                      onSuccess: onSuccess, onError: onError)
    }
    
    public func terminateCrossRoomConnection() {
        manager.disconnect()
    }
    
    public func registerConnectionObserver(observer: ConnectionObserver) {
        manager.addObserver(observer)
    }
    
    public func unregisterConnectionObserver(observer: ConnectionObserver) {
        manager.removerObserver(observer)
    }
    
    public func requestBattle(config: TUIBattleConfig, userIdList: [String],
                       timeout: TimeInterval, onSuccess: @escaping TUIBattleRequestBlock, onError: @escaping TUIErrorBlock) {
        manager.requestBattle(config: config, userIdList: userIdList, timeout: timeout, onSuccess: onSuccess, onError: onError)
    }
        
    public func cancelBattle(battleId: String, userIdList: [String], onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.cancelBattle(battleId: battleId, userIdList: userIdList, onSuccess: onSuccess, onError: onError)
    }

    public func respondToBattle(battleId: String, isAccepted: Bool, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        manager.respondToBattle(battleId: battleId, isAccepted: isAccepted, onSuccess: onSuccess, onError: onError)
    }

    public func terminateBattle(battleId: String, onSuccess: @escaping TUISuccessBlock, onError: @escaping TUIErrorBlock) {
        if !battleId.isEmpty {
            manager.terminateBattle(battleId: battleId, onSuccess: onSuccess, onError: onError)
        }
    }
    
    public func registerBattleObserver(observer: BattleObserver) {
        manager.addBattleObserver(observer)
    }
    
    public func unregisterBattleObserver(observer: BattleObserver) {
        manager.removerBattleObserver(observer)
    }
}

extension LiveCoreView {
    public func setLayoutMode(layoutMode: LayoutMode, layoutJson: String? = nil) {
        setLayout(layoutMode: layoutMode, layoutJson: layoutJson)
    }
}

// MARK: - Public manager
extension LiveCoreView {
    public var mediaManager: MediaManager {
        manager.context.mediaManager
    }
}

// MARK: - View layout logic.
extension LiveCoreView {
    private func initLayout() {
        setLayout(layoutMode: .gridLayout)
    }
    
    private func constructViewHierarchy() {
        addSubview(liveStreamContainerView)
        addSubview(layoutContainerView)
    }
    
    private func activateConstraints() {
        liveStreamContainerView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        layoutContainerView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func subscribeState() {
        manager.subscribeCoGuestState(StateSelector(keyPath: \CoGuestState.connectedUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] coGuestUsers in
                guard let self = self else { return }
                self.onCoGuestUserListChanged(coGuestUsers: coGuestUsers)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeCoGuestState(StateSelector(keyPath: \CoGuestState.coGuestStatus))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] coGuestStatus in
                guard let self = self else { return }
                guard let waitingCoGuestViewDelegate = self.waitingCoGuestViewDelegate else { return }
                switch coGuestStatus {
                    case .applying:
                        waitingCoGuestView = waitingCoGuestView ?? waitingCoGuestViewDelegate.waitingCoGuestView()
                        self.liveStreamContainerView.addView(waitingCoGuestView ?? UIView())
                    case .linking:
                        self.manager.updateVideoLayout(layout: nil)
                        guard let waitingCoGuestView = waitingCoGuestView else { return }
                        self.liveStreamContainerView.removeView(waitingCoGuestView)
                        self.waitingCoGuestView = nil
                    case .none:
                        guard let waitingCoGuestView = waitingCoGuestView else { return }
                        self.liveStreamContainerView.removeView(waitingCoGuestView)
                        self.waitingCoGuestView = nil
                }
            }
            .store(in: &cancellableSet)
        
        manager.subscribeCoHostState(StateSelector(keyPath: \CoHostState.connectedUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] coHostUsers in
                guard let self = self else { return }
                self.onCoHostUserListChanged(coHostUsers: coHostUsers)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeRoomState(StateSelector(keyPath: \RoomState.liveStatus))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] status in
                guard let self = self else { return }
                if status == .none {
                    self.liveStreamContainerView.removeAllViews()
                }
            }
            .store(in: &cancellableSet)
        
        manager.subscribeMediaState(StateSelector(keyPath: \MediaState.isCameraOpened))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isCameraOpened in
                guard let self = self else { return }
                isCameraOpened ? addLocalVideoView() : removeLocalVideoView()
            }
            .store(in: &cancellableSet)
        
        manager.subscribeLayoutState(StateSelector(keyPath: \LayoutState.videoLayout))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] videoLayout in
                guard let self = self else { return }
                self.onVideoLayoutChanged(layoutList: videoLayout?.layoutList ?? [],
                                          pixelScale: videoLayout?.pixelScale ?? 0)
            }
            .store(in: &cancellableSet)
        
        manager.subscribeBattleState(StateSelector(keyPath: \BattleState.isBattleStart))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isBattleStart in
                guard let self = self else { return }
                if isBattleStart {
                    onBattleStart()
                }
            }
            .store(in: &cancellableSet)
    }
    
}

extension LiveCoreView {
    private func addLocalVideoView() {
        let localVideoView = viewModel.getLocalLiveView()
        manager.setLocalVideoView(view: localVideoView.videoView)
        if !liveStreamContainerView.containsView(localVideoView) {
            if let videoViewDelegate = videoViewDelegate {
                let selfInfo = manager.userState.selfInfo
                let selfUserId = selfInfo.userId
                
                let coGuestWidgetsView = videoViewDelegate.createCoGuestView(userInfo: manager.userState.selfInfo)
                localVideoView.setCoGuestView(coGuestWidgetsView)
                let videoViewModel = VideoViewModel()
                videoViewModel.coGuestUser = selfInfo
                videoViewModel.userView = coGuestWidgetsView
                videoViewModelMap[selfUserId] = videoViewModel
                
                let hasVideo = manager.userState.hasVideoStreamUserList.contains(selfUserId) ||
                manager.mediaState.isCameraOpened
                let hasAudio = manager.userState.hasAudioStreamUserList.contains(selfUserId) ||
                !manager.mediaState.isMicrophoneMuted
                let coHostUser = LiveStreamConvert.convertToCoHostUser(userInfo: selfInfo, roomId: manager.roomState.roomId, hasVideoStream: hasVideo, hasAudioStream: hasAudio)
                let coHostWidgetsView = videoViewDelegate.createCoHostView(coHostUser: coHostUser)
                localVideoView.setCoHostView(coHostWidgetsView)
            }
            liveStreamContainerView.addView(localVideoView)
        }
    }
    
    private func removeLocalVideoView() {
        let localVideoView = viewModel.getLocalLiveView()
        manager.setLocalVideoView(view: nil)
        if liveStreamContainerView.containsView(localVideoView) {
            liveStreamContainerView.removeView(localVideoView)
            viewModel.clearLocalLiveView()
        }
        videoViewModelMap.removeValue(forKey: manager.userState.selfInfo.userId)
    }
    
    private func addCoGuestLiveView(userInfo: TUIUserInfo) {
        if userInfo.userId == manager.userState.selfInfo.userId {
            return
        }
        let liveView = viewModel.getRemoteLiveViewByUserId(userId: userInfo.userId)
        manager.setRemoteVideoView(userId: userInfo.userId, streamType: .cameraStream, view: liveView.videoView)
        if !liveStreamContainerView.containsView(liveView) {
            if let videoViewDelegate = videoViewDelegate,
               !userInfo.userId.hasSuffix(manager.context.roomManager.mixStreamIdSuffix) {
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
        if userInfo.userId == manager.userState.selfInfo.userId {
            return
        }
        let liveView = viewModel.getRemoteLiveViewByUserId(userId: userInfo.userId)
        viewModel.removeRemoteView(userId: userInfo.userId)
        liveStreamContainerView.removeView(liveView)
        videoViewModelMap.removeValue(forKey: userInfo.userId)
    }
    
    private func addCoHostLiveView(user: TUIConnectionUser) {
        if user.userId == manager.userState.selfInfo.userId {
            return
        }
        let liveView = viewModel.getRemoteLiveViewByUserId(userId: user.userId)
        manager.setRemoteVideoView(userId: user.userId, streamType: .cameraStream, view: liveView.videoView)
        if !liveStreamContainerView.containsView(liveView) {
            if let videoViewDelegate = videoViewDelegate {
                let hasVideo = manager.userState.hasVideoStreamUserList.contains(user.userId)
                let hasAudio = manager.userState.hasAudioStreamUserList.contains(user.userId)
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
        if user.userId == manager.userState.selfInfo.userId ||
            user.userId == manager.roomState.ownerInfo.userId {
            return
        }
        let liveView = viewModel.getRemoteLiveViewByUserId(userId: user.userId)
        viewModel.removeRemoteView(userId: user.userId)
        liveStreamContainerView.removeView(liveView)
        videoViewModelMap.removeValue(forKey: user.userId)
    }
    
    private func onCoGuestUserListChanged(coGuestUsers: [TUISeatInfo]) {
        let (addedUsers, removedUsers) = calculateCoGuestUserChanges(newList: coGuestUsers)
        for seatInfo in addedUsers {
            coGuestUserList.append(seatInfo)
            addCoGuestLiveView(userInfo: LiveStreamConvert.convertToUserInfo(seatInfo: seatInfo))
            if seatInfo.userId == manager.userState.selfInfo.userId {
                addLocalVideoView()
            }
        }
        for seatInfo in removedUsers {
            coGuestUserList.removeAll { $0.userId == seatInfo.userId }
            removeCoGuestLiveView(userInfo: LiveStreamConvert.convertToUserInfo(seatInfo: seatInfo))
            if seatInfo.userId == manager.userState.selfInfo.userId {
                removeLocalVideoView()
            }
        }
    }
    
    private func onCoHostUserListChanged(coHostUsers: [TUIConnectionUser]) {
        if manager.context.coHostManager.hasMixStreamUser() {
            return
        }
        for user in coHostUsers {
            if manager.roomState.ownerInfo.userId == user.userId {
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
        
        coHostUserList.forEach { user in
            battleViewInfoMap.removeValue(forKey: user.userId)
        }
        
        if !manager.battleState.battledUsers.isEmpty &&
            liveStreamContainerView.getFullScreenWidgetsView().subviews.count == 0 {
            addBattleContainerView(container: liveStreamContainerView)
            addBattleView(container: liveStreamContainerView)
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
    
    private func setLayout(layoutMode: LayoutMode, layoutJson: String? = nil) {
        viewModel.setLayoutMode(layoutMode: layoutMode, layoutJson: layoutJson)
        guard let layoutConfig = viewModel.layoutConfig else { return }
        liveStreamContainerView.setLayoutConfig(layoutConfig: layoutConfig)
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
            videoViewDelegate.updateCoGuestView(coGuestView: userView,userInfo: coGuestUser, modifyFlag: modifyFlags)
        } else if let coHostUser = videoViewModel.coHostUser {
            coHostUser.hasAudioStream = hasAudio
            videoViewDelegate.updateCoHostView(coHostView: userView, coHostUser: coHostUser, modifyFlag: modifyFlags)
        }
    }
    
    func onUserVideoStateChanged(userId: String, hasVideo: Bool, reason: TUIChangeReason) {
        guard let videoViewDelegate = videoViewDelegate,
              let videoViewModel = videoViewModelMap[userId],
              let userView = videoViewModel.userView else { return }
        let modifyFlags: UserInfoModifyFlag = [.hasVideoStream]
        if let coGuestUser = videoViewModel.coGuestUser {
            coGuestUser.hasVideoStream = hasVideo
            videoViewDelegate.updateCoGuestView(coGuestView: userView, userInfo: coGuestUser, modifyFlag: modifyFlags)
        } else if let coHostUser = videoViewModel.coHostUser {
            coHostUser.hasAudioStream = hasVideo
            videoViewDelegate.updateCoHostView(coHostView: userView, coHostUser: coHostUser, modifyFlag: modifyFlags)
        }
    }
    
    func onUserInfoChanged(userInfo: TUIUserInfo, modifyFlag: TUIUserInfoModifyFlag) {
        guard let videoViewDelegate = videoViewDelegate,
              let videoViewModel = videoViewModelMap[userInfo.userId],
              let userView = videoViewModel.userView else { return }
        let modifyFlags: UserInfoModifyFlag = LiveStreamConvert.convertToUserInfoModifyFlag(flag: modifyFlag)
        if var coGuestUser = videoViewModel.coGuestUser {
            coGuestUser = userInfo
            videoViewDelegate.updateCoGuestView(coGuestView: userView, userInfo: coGuestUser, modifyFlag: modifyFlags)
        }
    }
}

class VideoViewModel {
    var coHostUser: CoHostUser?
    var coGuestUser: TUIUserInfo?
    var userView: UIView?
}

class BattleViewInfo {
    var battleUser: TUIBattleUser?
    var battleView: UIView?
}

// MARK: - LayoutInfoChanged
extension LiveCoreView {
    private func onVideoLayoutChanged(layoutList: [ViewInfo], pixelScale: CGFloat) {
        let height = layoutList.count > 1 ? UIScreen.main.bounds.width * pixelScale : UIScreen.main.bounds.height
        layoutContainerView.snp.remakeConstraints { make in
            make.width.equalToSuperview()
            make.height.equalTo(height)
            make.center.equalToSuperview()
        }
        
        videoLayoutList.removeAll()
        videoLayoutViewMap.removeAll()
        layoutContainerView.removeAllViews()
        
        let layoutConfig = [layoutList.count : LayoutInfo(backgroundColor: "", viewInfoList: layoutList)]
        layoutContainerView.setLayoutConfig(layoutConfig: layoutConfig)
        let (addedUsers, removedUsers) = calculateVideoLayoutChanges(newList: layoutList)
        for layoutInfo in addedUsers {
            videoLayoutList.append(layoutInfo)
            addVideoLayoutView(layoutInfo: layoutInfo)
        }
        for layoutInfo in removedUsers {
            videoLayoutList.removeAll { $0.userId == layoutInfo.userId }
            removeVideoLayoutView(layoutInfo: layoutInfo)
        }
        
        videoLayoutList.forEach { layoutInfo in
            battleViewInfoMap.removeValue(forKey: layoutInfo.userId)
        }
        
        if !manager.battleState.battledUsers.isEmpty &&
            layoutContainerView.getFullScreenWidgetsView().subviews.count == 0 {
            addBattleContainerView(container: layoutContainerView)
            addBattleView(container: layoutContainerView)
        }
    }
    
    private func calculateVideoLayoutChanges(newList: [ViewInfo]) -> (added: [ViewInfo], removed: [ViewInfo]) {
        let newSet = Set(newList.map { $0.userId })
        let oldSet = Set(videoLayoutList.map { $0.userId })
        
        let addedUsers = newList.filter { !oldSet.contains($0.userId) }
        let removedUsers = videoLayoutList.filter { !newSet.contains($0.userId) }
        
        return (addedUsers, removedUsers)
    }
    
    private func addVideoLayoutView(layoutInfo: ViewInfo) {
        if layoutInfo.userId == manager.userState.selfInfo.userId {
            return
        }
        let liveView = getVideoLayoutViewByUserId(userId: layoutInfo.userId)
        if !layoutContainerView.containsView(liveView) {
            if let videoViewDelegate = videoViewDelegate {
                let userInfo = TUIUserInfo()
                userInfo.userId = layoutInfo.userId
                userInfo.userName = layoutInfo.userId
                userInfo.avatarUrl = ""
                userInfo.hasVideoStream = true
                if manager.coHostState.connectedUserList.isEmpty {
                    if let layoutWidgetsView = videoViewDelegate.createCoGuestView(userInfo: userInfo) {
                        liveView.addSubview(layoutWidgetsView)
                        layoutWidgetsView.snp.makeConstraints { make in
                            make.edges.equalToSuperview()
                        }
                        debugPrint("createCoGuestView: frame: \(liveView.frame), userId: \(userInfo.userId)")
                    }
                } else {
                    
                    let hasAudio = manager.userState.hasAudioStreamUserList.contains(userInfo.userId)
                    let coHostUser = LiveStreamConvert.convertToCoHostUser(userInfo: userInfo, roomId: manager.roomState.roomId, hasVideoStream: userInfo.hasVideoStream, hasAudioStream: hasAudio)
                    if let layoutWidgetsView = videoViewDelegate.createCoHostView(coHostUser: coHostUser) {
                        liveView.addSubview(layoutWidgetsView)
                        layoutWidgetsView.snp.makeConstraints { make in
                            make.edges.equalToSuperview()
                        }
                        debugPrint("createCoHostView: frame: \(liveView.frame), userId: \(coHostUser.connectionUser.userId)")
                    }
                }
            }
            layoutContainerView.addView(liveView)
        }
    }
    
    private func removeVideoLayoutView(layoutInfo: ViewInfo) {
        if layoutInfo.userId == manager.userState.selfInfo.userId {
            return
        }
        let liveView = getVideoLayoutViewByUserId(userId: layoutInfo.userId)
        layoutContainerView.removeView(liveView)
        videoLayoutViewMap.removeValue(forKey: layoutInfo.userId)
    }
    
    func getVideoLayoutViewByUserId(userId: String) -> UIView {
        if let layoutView = videoLayoutViewMap[userId] {
            return layoutView
        } else {
            let layoutView = UIView()
            videoLayoutViewMap[userId] = layoutView
            return layoutView
        }
    }
}

// MARK: - BattleStateSubscribe
extension LiveCoreView {
    private func onBattleStart() {
        removeAllBattleView()
        if manager.context.coHostManager.hasMixStreamUser() {
            if videoLayoutList.isEmpty {
                // wait for onLiveVideoLayoutChanged
                LiveStreamLog.info("\(#file)","\(#line)", "LiveCoreView onBattleStarted, wait for onLiveVideoLayoutChanged")
            } else {
                addBattleContainerView(container: layoutContainerView)
                addBattleView(container: layoutContainerView)
            }
        } else {
            addBattleContainerView(container: liveStreamContainerView)
            addBattleView(container: liveStreamContainerView)
        }
    }
    
    private func addBattleContainerView(container: LiveStreamViewContainer) {
        let fullScreenView = container.getFullScreenWidgetsView()
        fullScreenView.isHidden = false
        
        if let videoViewDelegate = videoViewDelegate, let battleContainerView = videoViewDelegate.createBattleContainerView() {
            fullScreenView.addSubview(battleContainerView)
            battleContainerView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        }
    }
    
    private func addBattleView(container: LiveStreamViewContainer) {
        var itemView: UIView?
        let battleUsers = manager.battleState.battledUsers
        for battleUser in battleUsers {
            if container == liveStreamContainerView {
                if battleUser.userId == manager.userState.selfInfo.userId {
                    itemView = viewModel.getLocalLiveView()
                } else {
                    itemView = viewModel.getRemoteLiveViewByUserId(userId: battleUser.userId)
                }
            } else if container == layoutContainerView {
                itemView = videoLayoutViewMap[battleUser.userId]
            }
            
            guard let itemView = itemView, container.containsView(itemView) else { continue }
            
            if let videoViewDelegate = videoViewDelegate, let battleView = videoViewDelegate.createBattleView(battleUser: battleUser) {
                let battleViewInfo = BattleViewInfo()
                battleViewInfo.battleUser = battleUser
                battleViewInfo.battleView = battleView
                battleViewInfoMap.updateValue(battleViewInfo, forKey: battleUser.userId)
                
                itemView.addSubview(battleView)
                battleView.snp.makeConstraints { make in
                    make.edges.equalToSuperview()
                }
                container.layoutIfNeeded()
            }
        }
        updateBattleContainerView(container: container)
    }
    
    private func updateBattleContainerView(container: LiveStreamViewContainer) {
        guard let videoViewDelegate = videoViewDelegate, container.getFullScreenWidgetsView().subviews.count != 0 else { return }
        guard let battleContainerView = container.getFullScreenWidgetsView().subviews.first else { return }
        var userInfos: [BattleUserViewModel] = []
        battleViewInfoMap.forEach { (userId, battleViewInfo) in
            if let battleUser = battleViewInfo.battleUser,
               let battleView = battleViewInfo.battleView,
               battleView.superview != nil {
                let model = BattleUserViewModel()
                model.battleUser = battleUser
                model.rect = battleView.superview?.frame ?? .zero
                userInfos.append(model)
            }
        }
        videoViewDelegate.updateBattleContainerView(battleContainerView: battleContainerView, userInfos: userInfos)
    }
    
    private func removeAllBattleView() {
        liveStreamContainerView.removeFullScreenWidgetsViews()
        layoutContainerView.removeFullScreenWidgetsViews()
        
        battleViewInfoMap.forEach { (_, viewInfo) in
            guard let battleView = viewInfo.battleView else { return }
            battleView.removeFromSuperview()
        }
    }
}
