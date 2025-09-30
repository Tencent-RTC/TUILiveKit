//
//  AudienceUserManagePanelView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/2/17.
//

import RTCRoomEngine
import AtomicXCore
import RTCCommon
import Combine
import ImSDK_Plus

class AudienceUserManagePanelView: RTCBaseView {
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
    private weak var coreView: LiveCoreView?
    private let userManagePanelType: AudienceUserManagePanelType
    private var user: TUIUserInfo
    @Published private var isFollow: Bool = false
    
    private var isSelf: Bool {
        user.userId == manager.coreUserState.selfInfo.userId
    }
    private var isOwner: Bool {
        user.userId == manager.coreRoomState.ownerInfo.userId
    }
    private var isSelfMuted: Bool {
        manager.coreMediaState.isMicrophoneMuted
    }
    private var isSelfCameraOpened: Bool {
        manager.coreMediaState.isCameraOpened
    }
    private var isSelfOnSeat: Bool {
        manager.coGuestState.coGuestStatus == .linking
    }
    private var isAudioLocked: Bool {
        return manager.coGuestState.lockAudioUserList.contains(user.userId)
    }
    private var isCameraLocked: Bool {
        return manager.coGuestState.lockVideoUserList.contains(user.userId)
    }
    private var cancellableSet = Set<AnyCancellable>()
    
    public init(user: TUIUserInfo, manager: AudienceManager, routerManager: AudienceRouterManager, coreView: LiveCoreView, type: AudienceUserManagePanelType) {
        self.user = user
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
        self.userManagePanelType = type
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("deinit \(self)")
    }
    
    private lazy var userInfoView: UIView = {
        let view = UIView()
        return view
    }()
    
    private lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.layer.cornerRadius = 20.scale375()
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    private lazy var userNameLabel: UILabel = {
        let label = UILabel()
        label.text = user.userName.isEmpty ? user.userId : user.userName
        label.font = .customFont(ofSize: 16)
        label.textColor = .g7
        return label
    }()
    
    private lazy var idLabel: UILabel = {
        let label = UILabel()
        label.font = .customFont(ofSize: 12)
        label.text = "ID: " + user.userId
        label.textColor = .greyColor
        return label
    }()
    
    private lazy var followButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = .deepSeaBlueColor
        button.titleLabel?.font = .customFont(ofSize: 14)
        button.setTitle(.followText, for: .normal)
        button.setTitleColor(.g7, for: .normal)
        button.setImage(internalImage("live_user_followed_icon"), for: .selected)
        button.layer.cornerRadius = 16
        button.isHidden = isSelf
        return button
    }()
    
    private lazy var featureClickPanel: AudienceFeatureClickPanel = {
        let model = generateFeatureClickPanelModel()
        let featureClickPanel = AudienceFeatureClickPanel(model: model)
        return featureClickPanel
    }()

    override func constructViewHierarchy() {
        self.layer.masksToBounds = true
        addSubview(userInfoView)
        userInfoView.addSubview(avatarImageView)
        userInfoView.addSubview(userNameLabel)
        userInfoView.addSubview(idLabel)
        userInfoView.addSubview(followButton)
        addSubview(featureClickPanel)
    }
    
    override func activateConstraints() {
        userInfoView.snp.makeConstraints { make in
            make.leading.trailing.top.equalToSuperview().inset(24)
            make.height.equalTo(43.scale375())
        }
        avatarImageView.snp.makeConstraints { make in
            make.leading.centerY.equalToSuperview()
            make.width.height.equalTo(40.scale375())
        }
        userNameLabel.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(avatarImageView.snp.trailing).offset(12.scale375())
            make.height.equalTo(20.scale375())
            make.width.lessThanOrEqualTo(170.scale375())
        }
        idLabel.snp.makeConstraints { make in
            make.leading.equalTo(userNameLabel)
            make.top.equalTo(userNameLabel.snp.bottom).offset(5.scale375())
            make.height.equalTo(17.scale375())
            make.width.lessThanOrEqualTo(200.scale375())
        }
        followButton.snp.makeConstraints{ make in
            make.trailing.centerY.equalToSuperview()
            make.width.equalTo(67.scale375())
            make.height.equalTo(32.scale375())
        }
        featureClickPanel.snp.makeConstraints{ make in
            make.top.equalTo(userInfoView.snp.bottom).offset(21.scale375())
            make.leading.equalTo(userInfoView)
            make.bottom.equalToSuperview()
        }
    }
    
    override func bindInteraction() {
        subscribeState()
        followButton.addTarget(self, action: #selector(followButtonClick), for: .touchUpInside)
    }
    
    override func setupViewStyle() {
        backgroundColor = .g2
        layer.cornerRadius = 12
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        avatarImageView.kf.setImage(with: URL(string: user.avatarUrl), placeholder: UIImage.avatarPlaceholderImage)
        checkFollowStatus()
    }
    
    private func subscribeState() {
        $isFollow.receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isFollow in
                guard let self = self else { return }
                if isFollow {
                    followButton.backgroundColor = .g5
                    followButton.setTitle("", for: .normal)
                    followButton.setImage(internalImage("live_user_followed_icon"), for: .normal)
                } else {
                    followButton.backgroundColor = .deepSeaBlueColor
                    followButton.setTitle(.followText, for: .normal)
                    followButton.setImage(nil, for: .normal)
                }
            }
            .store(in: &cancellableSet)
        
        let isCameraOpenedPublisher = manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \MediaState.isCameraOpened))
        let isMicrophoneMutedPublisher = manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \MediaState.isMicrophoneMuted))
        let isAudioLockedPublisher = manager.subscribeState(StateSelector(keyPath: \AudienceMediaState.isAudioLocked))
        let isVideoLockedPublisher = manager.subscribeState(StateSelector(keyPath: \AudienceMediaState.isVideoLocked))
        let lockAudioUserListPublisher = manager.subscribeState(StateSelector(keyPath: \AudienceCoGuestState.lockAudioUserList))
        let lockVideoUserListPublisher = manager.subscribeState(StateSelector(keyPath: \AudienceCoGuestState.lockVideoUserList))
        
        isCameraOpenedPublisher.removeDuplicates()
            .combineLatest(isMicrophoneMutedPublisher.removeDuplicates(),
                           isAudioLockedPublisher.removeDuplicates(),
                           isVideoLockedPublisher.removeDuplicates())
            .receive(on: RunLoop.main)
            .sink { [weak self] isCameraOpened in
                guard let self = self, isSelf else { return }
                updateFeatureItems()
            }
            .store(in: &cancellableSet)
        
        lockAudioUserListPublisher.removeDuplicates()
            .combineLatest(lockVideoUserListPublisher.removeDuplicates())
            .receive(on: RunLoop.main)
            .sink { [weak self] isCameraOpened in
                guard let self = self else { return }
                updateFeatureItems()
            }
            .store(in: &cancellableSet)
        
        manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \CoGuestState.connectedUserList))
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] seatList in
                guard let self = self, userManagePanelType == .mediaAndSeat,
                      user.userId != manager.coreRoomState.ownerInfo.userId else { return }
                if !seatList.contains(where: { $0.userId == self.user.userId }) {
                    routerManager.router(action: .dismiss())
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func checkFollowStatus() {
        V2TIMManager.sharedInstance().checkFollowType(userIDList: [user.userId]) { [weak self] checkResultList in
            guard let self = self, let result = checkResultList?.first else { return }
            if result.followType == .FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST || result.followType == .FOLLOW_TYPE_IN_MY_FOLLOWING_LIST {
                self.isFollow = true
            } else {
                self.isFollow = false
            }
        } fail: { _, _ in
        }
    }
    
    private func generateFeatureClickPanelModel() -> AudienceFeatureClickPanelModel {
        let model = AudienceFeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 56.scale375Height())
        model.itemDiff = 12.scale375()
        switch userManagePanelType {
        case .mediaAndSeat:
            if isSelf {
                model.items.append(muteSelfAudioItem)
                model.items.append(closeSelfCameraItem)
                if isSelfCameraOpened {
                    model.items.append(flipItem)
                }
                if !isOwner && isSelfOnSeat {
                    model.items.append(leaveSeatItem)
                }
            }
            break
        case .userInfo:
            break
        }
        return model
    }
    
    private func updateFeatureItems() {
        if isSelf {
            muteSelfAudioItem.isSelected = isSelfMuted
            muteSelfAudioItem.isDisabled = isAudioLocked
            closeSelfCameraItem.isSelected = !isSelfCameraOpened
            closeSelfCameraItem.isDisabled = isCameraLocked
        }
        let newItems = generateFeatureClickPanelModel().items
        featureClickPanel.updateFeatureItems(newItems: newItems)
    }
    
    private lazy var designConfig: AudienceFeatureItemDesignConfig = {
        var designConfig = AudienceFeatureItemDesignConfig()
        designConfig.type = .imageAboveTitleBottom
        designConfig.imageTopInset = 14.scale375()
        designConfig.imageLeadingInset = 14.scale375()
        designConfig.imageSize = CGSize(width: 28.scale375(), height: 28.scale375())
        designConfig.titileColor = .g7
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.backgroundColor = .g3.withAlphaComponent(0.3)
        designConfig.cornerRadius = 8.scale375Width()
        designConfig.titleHeight = 20.scale375Height()
        return designConfig
    }()
    
    private lazy var muteSelfAudioItem: AudienceFeatureItem = {
        AudienceFeatureItem(normalTitle: .muteAudioText,
                      normalImage: internalImage("live_anchor_unmute_icon"),
                      selectedTitle: .unmuteAudioText,
                      selectedImage: internalImage("live_anchor_mute_icon"),
                      isSelected: isSelfMuted,
                      isDisabled: isAudioLocked,
                      designConfig: designConfig,
                      actionClosure: { [weak self] sender in
            guard let self = self else { return }
            self.muteSelfAudioClick(sender)
        })
    }()
    
    private lazy var closeSelfCameraItem: AudienceFeatureItem = {
        AudienceFeatureItem(normalTitle: .closeCameraText,
                      normalImage: internalImage("live_open_camera_icon"),
                      selectedTitle: .opneCameraText,
                      selectedImage: internalImage("live_close_camera_icon"),
                      isSelected: !isSelfCameraOpened,
                      isDisabled: isCameraLocked,
                      designConfig: designConfig,
                      actionClosure: { [weak self] sender in
            guard let self = self else { return }
            self.closeSelfCameraClick(sender)
        })
    }()
    
    private lazy var flipItem: AudienceFeatureItem = {
        AudienceFeatureItem(normalTitle: .filpText,
                      normalImage: internalImage("live_video_setting_flip"),
                      designConfig: designConfig,
                      actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.flipClick()
        })
    }()
    
    private lazy var leaveSeatItem: AudienceFeatureItem = {
        AudienceFeatureItem(normalTitle: .disconnectText,
                      normalImage: internalImage("live_leave_seat_icon"),
                      designConfig: designConfig,
                      actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.leaveSeatClick()
        })
    }()
}

// MARK: - Action
extension AudienceUserManagePanelView {
    @objc private func followButtonClick() {
        if isFollow {
            V2TIMManager.sharedInstance().unfollowUser(userIDList: [user.userId]) { [weak self] followResultList in
                guard let self = self, let result = followResultList?.first else { return }
                if result.resultCode == 0 {
                    isFollow = false
                } else {
                    manager.toastSubject.send("code: \(result.resultCode), message: \(String(describing: result.resultInfo))")
                }
            } fail: { [weak self] code, message in
                guard let self = self else { return }
                manager.toastSubject.send("code: \(code), message: \(String(describing: message))")
            }
        } else {
            V2TIMManager.sharedInstance().followUser(userIDList: [user.userId]) { [weak self] followResultList in
                guard let self = self, let result = followResultList?.first else { return }
                if result.resultCode == 0 {
                    isFollow = true
                } else {
                    manager.toastSubject.send("code: \(result.resultCode), message: \(String(describing: result.resultInfo))")
                }
            } fail: { [weak self] code, message in
                guard let self = self else { return }
                manager.toastSubject.send("code: \(code), message: \(String(describing: message))")
            }
        }
    }
    
    private func muteSelfAudioClick(_ sender: AudienceFeatureItemButton) {
        if isSelfMuted {
            coreView?.unmuteMicrophone { [weak sender, weak self] in
                guard let self = self, let sender = sender else { return }
                sender.isSelected = isSelfMuted
            } onError: { [weak self] code, message in
                guard let self = self else { return }
                let err = InternalError(code: code.rawValue, message: message)
                manager.toastSubject.send(err.localizedMessage)
            }
        } else {
            coreView?.muteMicrophone()
            sender.isSelected = !sender.isSelected
        }
        routerManager.router(action: .dismiss())
    }
    
    private func closeSelfCameraClick(_ sender: AudienceFeatureItemButton) {
        if isSelfCameraOpened {
            coreView?.stopCamera()
            sender.isSelected = !sender.isSelected
        } else {
            coreView?.startCamera(useFrontCamera: manager.coreMediaState.isFrontCamera) { [weak sender, weak self] in
                guard let self = self, let sender = sender else { return }
                sender.isSelected = !isSelfCameraOpened
            } onError: { [weak self] code, message in
                guard let self = self else { return }
                let err = InternalError(code: code.rawValue, message: message)
                manager.toastSubject.send(err.localizedMessage)
            }
        }
        routerManager.router(action: .dismiss())
    }
    
    private func flipClick() {
        coreView?.switchCamera(isFront: !manager.coreMediaState.isFrontCamera)
        routerManager.router(action: .dismiss())
    }
    
    private func leaveSeatClick() {
        let alertInfo = AudienceAlertInfo(description: .leaveSeatAlertText, imagePath: nil,
                                    cancelButtonInfo: (.cancelText, .g3),
                                    defaultButtonInfo: (.disconnectText, .redColor)) { alertPanel in
            alertPanel.dismiss()
        } defaultClosure: { [weak self] alertPanel in
            guard let self = self else { return }
            coreView?.terminateIntraRoomConnection()
            alertPanel.dismiss()
            routerManager.router(action: .dismiss())
        }
        let alertPanel = AudienceAlertPanel(alertInfo: alertInfo)
        alertPanel.show()
    }
}

fileprivate extension String {
    static let followText = internalLocalized("Follow")
    static let disableChatText = internalLocalized("Disable Chat")
    static let enableChatText = internalLocalized("Enable Chat")
    static let kickOutOfRoomText = internalLocalized("Remove Out")
    static let kickOutOfRoomConfirmText = internalLocalized("Remove")
    static let kickOutAlertText = internalLocalized("Are you sure you want to remove xxx?")
    static let muteAudioText = internalLocalized("Mute")
    static let unmuteAudioText = internalLocalized("Unmute")
    static let opneCameraText = internalLocalized("Start Video")
    static let closeCameraText = internalLocalized("Stop Video")
    static let filpText = internalLocalized("Flip")
    static let leaveSeatAlertText = internalLocalized("Are you sure you want to disconnect?")
    static let cancelText = internalLocalized("Cancel")
    static let disableAudioText = internalLocalized("Disable Audio")
    static let enableAudioText = internalLocalized("Enable Audio")
    static let disableCameraText = internalLocalized("Disable Video")
    static let enableCameraText = internalLocalized("Enable Video")
    static let hangupText = internalLocalized("End")
    static let hangupAlertText = internalLocalized("Are you sure you want to disconnect xxx?")
    static let disconnectText = internalLocalized("End Co-guest")
}
