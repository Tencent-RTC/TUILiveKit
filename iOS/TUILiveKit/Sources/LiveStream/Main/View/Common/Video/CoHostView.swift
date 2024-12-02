//
//  CoHostView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/25.
//

import Foundation
import RTCCommon
import Combine
import RTCRoomEngine

class CoHostView: UIView {
    private let manager: LiveStreamManager
    private var isViewReady: Bool = false
    private var connectionUser: TUIConnectionUser
    private var cancellableSet = Set<AnyCancellable>()
    
    init(connectionUser: TUIConnectionUser, manager: LiveStreamManager) {
        self.connectionUser = connectionUser
        self.manager = manager
        super.init(frame: .zero)
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
        initViewState()
        subscribeState()
    }
    
    private lazy var userInfoView: UserStatusView = {
        let view = UserStatusView(userInfo: LSUser(connectionUser: connectionUser), manager: manager)
        return view
    }()
    
    private lazy var battleMemberInfoView: BattleMemberInfoView = {
        let view = BattleMemberInfoView(manager: manager, userId: connectionUser.userId)
        return view
    }()
    
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
        addSubview(battleMemberInfoView)
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
        battleMemberInfoView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func initViewState() {
        if connectionUser.userId == manager.userState.selfInfo.userId {
            userInfoView.isHidden = true
        }
        avatarImageView.kf.setImage(with: URL(string: connectionUser.avatarUrl),
                                    placeholder: UIImage.avatarPlaceholderImage)
        let hasVideo = manager.userState.hasVideoStreamUserList.contains(connectionUser.userId)
        let isPreview = manager.roomState.liveStatus == .previewing
        avatarImageView.isHidden = hasVideo || isPreview
    }
}

extension CoHostView {
    
    func subscribeState() {
        manager.subscribeUserState(StateSelector(keyPath: \LSUserState.hasVideoStreamUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] userIdList in
                guard let self = self, manager.roomState.liveStatus != .previewing else { return }
                if userIdList.contains(self.connectionUser.userId) {
                    avatarImageView.isHidden = true
                } else {
                    avatarImageView.isHidden = false
                }
            }
            .store(in: &cancellableSet)
    }
}
