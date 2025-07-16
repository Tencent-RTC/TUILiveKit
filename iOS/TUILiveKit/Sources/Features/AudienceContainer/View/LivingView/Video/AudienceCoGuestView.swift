//
//  AudienceCoGuestView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/25.
//

import Foundation
import Kingfisher
import Combine
import RTCRoomEngine
import RTCCommon
import LiveStreamCore

class AudienceCoGuestView: UIView {
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
    private var cancellableSet = Set<AnyCancellable>()
    private var isViewReady: Bool = false
    private var userInfo: TUIUserInfo
    
    init(userInfo: TUIUserInfo, manager: AudienceManager, routerManager: AudienceRouterManager) {
        self.userInfo = userInfo
        self.manager = manager
        self.routerManager = routerManager
        super.init(frame: .zero)
        let tapGestureRecognizer = UITapGestureRecognizer(target: self, action: #selector(handleTap))
        addGestureRecognizer(tapGestureRecognizer)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
        initViewState()
    }
    
    private lazy var userInfoView = AudienceUserStatusView(userInfo: userInfo, manager: manager)
    
    private lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 40.scale375() * 0.5
        imageView.layer.masksToBounds = true
        imageView.isHidden = true
        return imageView
    }()
    
    private func constructViewHierarchy() {
        addSubview(userInfoView)
        addSubview(avatarImageView)
    }

    private func activateConstraints() {
        userInfoView.snp.makeConstraints { make in
            make.height.equalTo(18)
            make.bottom.equalToSuperview().offset(-5)
            make.leading.equalToSuperview().offset(5)
            make.width.lessThanOrEqualTo(self).multipliedBy(0.9)
        }
        avatarImageView.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(40.scale375())
            make.height.equalTo(40.scale375())
        }
    }
    
    private func initViewState() {
        avatarImageView.kf.setImage(with: URL(string: userInfo.avatarUrl),
                                    placeholder: UIImage.avatarPlaceholderImage)
        if userInfo.userId == manager.coreUserState.selfInfo.userId  ||
            (userInfo.userId == manager.coreRoomState.ownerInfo.userId &&
             manager.coGuestState.coGuestStatus == .none) {
            userInfoView.isHidden = true
        }
        let hasVideo = manager.coreUserState.hasVideoStreamUserList.contains(userInfo.userId)
        avatarImageView.isHidden = hasVideo || userInfo.hasVideoStream || !isEnteredRoom()
    }
    
    @objc private func handleTap() {
        let isSelfOwner = manager.coreUserState.selfInfo.userRole == .roomOwner
        let isSelfView = userInfo.userId == manager.coreUserState.selfInfo.userId
        let isOnlyUserOnSeat = manager.coreCoGuestState.connectedUserList.count == 1
        if !isSelfOwner && isOnlyUserOnSeat && !isSelfView { return }
        let type: AudienceUserManagePanelType = !isSelfOwner && !isSelfView ? .userInfo : .mediaAndSeat
        routerManager.router(action: .present(.userManagement(userInfo, type: type)))
    }
    
    private func isEnteredRoom() -> Bool {
        return manager.roomState.liveStatus != .none
    }
}

extension AudienceCoGuestView {
    func subscribeState() {
        manager.subscribeCoreViewState(StateSelector(keyPath: \UserState.hasVideoStreamUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] userIdList in
                guard let self = self else { return }
                if userIdList.contains(self.userInfo.userId) || self.userInfo.hasVideoStream || !isEnteredRoom() {
                    avatarImageView.isHidden = true
                } else {
                    avatarImageView.isHidden = false
                }
            }
            .store(in: &cancellableSet)
        
        FloatWindow.shared.subscribeShowingState()
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isShow in
                guard let self = self else { return }
                isHidden = isShow
            }
            .store(in: &cancellableSet)
    }
}
