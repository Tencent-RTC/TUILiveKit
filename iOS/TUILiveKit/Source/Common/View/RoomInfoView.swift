//
//  RoomInfoView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2024/5/8.
//

import Foundation
import Combine
import Kingfisher

class RoomInfoView: UIControl {
    var store: LiveStore
    private lazy var followingUserList = self.store.select(UserSelectors.getMyFollowingList)
    private lazy var roomOwnerInfo = self.store.select(RoomSelectors.getRoomOwnerInfo)
    private lazy var isOwner = self.store.select(UserSelectors.isOwner)
    private var cancellableSet = Set<AnyCancellable>()
    
    private lazy var roomOwnerNameLabel: UILabel = {
        let view = UILabel()
        view.font = .customFont(ofSize: 14, weight: .semibold)
        view.textColor = .g8
        view.textAlignment = .left
        return view
    }()

    private lazy var imageView: UIImageView = {
        let view = UIImageView(frame: CGRect(origin: .zero, size: CGSize(width: 24.scale375(), height: 24.scale375())))
        view.layer.cornerRadius = view.frame.width * 0.5
        view.layer.masksToBounds = true
        view.layer.borderColor = UIColor.cyanColor.cgColor
        view.layer.borderWidth = 0.86
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

    init(store: LiveStore) {
        self.store = store
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
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
        if !store.selectCurrent(UserSelectors.isOwner) {
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
            if !store.selectCurrent(UserSelectors.isOwner) {
                make.trailing.equalTo(followButton.snp.leading).offset(-8.scale375())
            } else {
                make.trailing.equalToSuperview().inset(8.scale375())
            }
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
        }
        if !store.selectCurrent(UserSelectors.isOwner) {
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
        subscribeTopViewState()
    }
    
    @objc func followButtonClick(_ sender: UIButton) {
        let roomOwnerId = store.selectCurrent(RoomSelectors.roomOwnerId)
        store.dispatch(action: sender.isSelected ? UserActions.unfollow(payload: roomOwnerId) : UserActions.follow(payload: roomOwnerId))
    }
    
    private func subscribeTopViewState() {
        followingUserList
            .receive(on: RunLoop.main)
            .sink { [weak self] userList in
                guard let self = self else { return }
                let userIdList = userList.map { $0.userId }
                if userIdList.contains(store.selectCurrent(RoomSelectors.roomOwnerId)) {
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
   
        roomOwnerInfo
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerInfo in
                guard let self = self else { return }
                self.imageView.kf.setImage(with: URL(string: ownerInfo.avatarUrl), placeholder: UIImage.placeholderImage)
                self.roomOwnerNameLabel.text = ownerInfo.name
            }
            .store(in: &cancellableSet)
        
        isOwner
            .receive(on: RunLoop.main)
            .sink { [weak self] isOwner in
                guard let self = self else { return }
                self.updateFollowButtonVisibility(visible: !isOwner)
            }
            .store(in: &cancellableSet)
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

fileprivate extension String {
    static let followText = localized("live.user.follow")
}
