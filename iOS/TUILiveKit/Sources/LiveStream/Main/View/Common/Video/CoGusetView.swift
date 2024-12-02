//
//  CoGuestView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/25.
//

import Foundation
import Kingfisher
import Combine
import RTCRoomEngine
import RTCCommon

class CoGuestView: UIView {
    private let manager: LiveStreamManager
    private var cancellableSet = Set<AnyCancellable>()
    private var isViewReady: Bool = false
    private var userInfo: TUIUserInfo
    
    init(userInfo: TUIUserInfo, manager: LiveStreamManager) {
        self.userInfo = userInfo
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
        subscribeState()
        initViewState()
    }
    
    private lazy var userInfoView: UserStatusView = {
        let view = UserStatusView(userInfo: LSUser(userInfo: userInfo), manager: manager)
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
        if userInfo.userId == manager.userState.selfInfo.userId {
            userInfoView.isHidden = true
        }
        let hasVideo = manager.userState.hasVideoStreamUserList.contains(userInfo.userId)
        let isPreview = manager.roomState.liveStatus == .previewing
        avatarImageView.isHidden = hasVideo || isPreview
    }
}

extension CoGuestView {
    
    func subscribeState() {
        manager.subscribeUserState(StateSelector(keyPath: \LSUserState.hasVideoStreamUserList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] userIdList in
                guard let self = self, manager.roomState.liveStatus != .previewing else { return }
                if userIdList.contains(self.userInfo.userId) {
                    avatarImageView.isHidden = true
                } else {
                    avatarImageView.isHidden = false
                }
            }
            .store(in: &cancellableSet)
    }
}

