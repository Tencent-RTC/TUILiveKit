//
//  LiveInfoView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2024/5/8.
//

import Foundation
import Combine
import Kingfisher
import RTCCommon

class LiveInfoView: UIView {
    private let service = RoomInfoService()
    var state: RoomInfoState {
        service.state
    }
    var isOwner: Bool {
        state.selfUserId == state.ownerId
    }
    private lazy var roomInfoPanelView = RoomInfoPanelView(service: service)
    private var cancellableSet = Set<AnyCancellable>()
    
    private lazy var roomOwnerNameLabel: UILabel = {
        let view = UILabel()
        view.font = .customFont(ofSize: 14, weight: .semibold)
        view.textColor = .g8
        view.textAlignment = .left
        view.isUserInteractionEnabled = false
        return view
    }()

    private lazy var imageView: UIImageView = {
        let view = UIImageView(frame: CGRect(origin: .zero, size: CGSize(width: 24.scale375(), height: 24.scale375())))
        view.layer.cornerRadius = view.frame.width * 0.5
        view.layer.masksToBounds = true
        view.layer.borderColor = UIColor.cyanColor.cgColor
        view.layer.borderWidth = 0.86
        view.isUserInteractionEnabled = false
        return view
    }()
    
    private let followButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = .deepSeaBlueColor
        button.titleLabel?.font = .customFont(ofSize: 12)
        button.setTitle(.followText, for: .normal)
        button.setTitleColor(.g7, for: .normal)
        button.setImage(.liveBundleImage("live_user_followed_icon"), for: .selected)
        button.layer.cornerRadius = 12.scale375()
        return button
    }()
    
    func initialize(roomId: String) {
        service.initRoomInfo(roomId: roomId)
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }

    private func constructViewHierarchy() {
        addSubview(roomOwnerNameLabel)
        addSubview(imageView)
        if !isOwner {
            addSubview(followButton)
        }
    }

    private func activateConstraints() {
        imageView.snp.makeConstraints { make in
            make.size.equalTo(imageView.frame.size)
            make.leading.equalToSuperview().inset(4.scale375())
            make.centerY.equalToSuperview()
        }

        roomOwnerNameLabel.snp.makeConstraints { make in
            make.leading.equalTo(imageView.snp.trailing).offset(8.scale375())
            if state.selfUserId != state.ownerId {
                make.trailing.equalTo(followButton.snp.leading).offset(-8.scale375())
            } else {
                make.trailing.equalToSuperview().inset(8.scale375())
            }
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
        }
        if !isOwner {
            followButton.snp.makeConstraints { make in
                make.trailing.equalToSuperview().inset(8.scale375())
                make.centerY.equalToSuperview()
                make.width.equalTo(45.scale375())
                make.height.equalTo(24.scale375Height())
            }
        }
    }
    
    private func bindInteraction() {
        followButton.addTarget(self, action: #selector(followButtonClick(_:)), for: .touchUpInside)
        subscribeRoomInfoState()
    }
    
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        containerTapAction()
    }
    
    @objc func followButtonClick(_ sender: UIButton) {
        if state.followingList.contains(where: { $0.userId == state.ownerId }) {
            service.unfollowUser(userId: state.ownerId)
        } else {
            service.followUser(userId: state.ownerId)
        }
    }
    
    private func subscribeRoomInfoState() {
        state.$followingList
            .receive(on: RunLoop.main)
            .sink { [weak self] userList in
                guard let self = self else { return }
                let userIdList = userList.map { $0.userId }
                if userIdList.contains(state.ownerId) {
                    self.followButton.isSelected = true
                    self.followButton.backgroundColor = .g5
                    self.followButton.setTitle("", for: .normal)
                    self.followButton.setImage(.liveBundleImage("live_user_followed_icon"), for: .selected)
                } else {
                    self.followButton.isSelected = false
                    self.followButton.backgroundColor = .deepSeaBlueColor
                    self.followButton.setTitle(.followText, for: .normal)
                    self.followButton.setImage(nil, for: .normal)
                }
            }
            .store(in: &cancellableSet)
   
        state.$ownerAvatarUrl
            .receive(on: RunLoop.main)
            .sink { [weak self] avatarUrl in
                guard let self = self else { return }
                self.imageView.kf.setImage(with: URL(string: avatarUrl), placeholder: UIImage.avatarPlaceholderImage)
            }
            .store(in: &cancellableSet)
        
        state.$ownerName
            .receive(on: RunLoop.main)
            .sink { [weak self] name in
                guard let self = self else { return }
                self.roomOwnerNameLabel.text = name
            }
            .store(in: &cancellableSet)
        
        state.$ownerId
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerId in
                guard let self = self else { return }
                self.updateFollowButtonVisibility(visible: ownerId != self.state.selfUserId)
                self.checkIsFollow(userId: ownerId)
            }
            .store(in: &cancellableSet)
    }
    
    private func checkIsFollow(userId: String) {
        service.isFollow(userId: userId)
    }
    
    private func updateFollowButtonVisibility(visible: Bool) {
        if visible {
            addSubview(followButton)
            followButton.snp.remakeConstraints { make in
                make.trailing.equalToSuperview().inset(8.scale375())
                make.centerY.equalToSuperview()
                make.width.equalTo(45.scale375())
                make.height.equalTo(24.scale375Height())
            }
            roomOwnerNameLabel.snp.remakeConstraints { make in
                make.leading.equalTo(imageView.snp.trailing).offset(8.scale375())
                make.trailing.equalTo(followButton.snp.leading).offset(-8.scale375())
                make.centerY.equalToSuperview()
                make.height.equalToSuperview()
            }
        } else {
            followButton.removeFromSuperview()
            roomOwnerNameLabel.snp.remakeConstraints { make in
                make.leading.equalTo(imageView.snp.trailing).offset(8.scale375())
                make.trailing.equalToSuperview().inset(8.scale375())
                make.centerY.equalToSuperview()
                make.height.equalToSuperview()
            }
        }
    }
}

extension LiveInfoView {
    @objc func containerTapAction() {
        if let vc = WindowUtils.getCurrentWindowViewController() {
            let menuContainerView = MenuContainerView(contentView: roomInfoPanelView)
            menuContainerView.blackAreaClickClosure = {
                vc.dismiss(animated: true)
            }
            let viewController = PopupViewController(contentView: menuContainerView)
            vc.present(viewController, animated: true)
        }
    }
}


fileprivate extension String {
    static let followText = localized("live.user.follow")
}
