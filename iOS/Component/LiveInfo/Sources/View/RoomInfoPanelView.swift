//
//  RoomInfoPanelView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/5/22.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine

class RoomInfoPanelView: RTCBaseView {
    private var cancellableSet = Set<AnyCancellable>()
    private let service: RoomInfoService
    private let state: RoomInfoState
    private var isOwner: Bool {
        state.ownerId == state.selfUserId
    }
    private let enableFollow: Bool
    
    private lazy var imageView: UIImageView = {
        let imageView = UIImageView()
        imageView.layer.cornerRadius = 55.scale375()/2
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    private let backgroundView: UIView = {
        let view = UIView()
        view.backgroundColor = .g2
        view.layer.cornerRadius = 12.scale375()
        view.layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        return view
    }()
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel()
        label.font = .customFont(ofSize: 16)
        label.textColor = .g7
        label.textAlignment = .center
        return label
    }()
    
    private lazy var roomIdLabel: UILabel = {
        let label = UILabel()
        label.font = .customFont(ofSize: 12)
        label.text = .localizedReplace(.roomIdText, replace: state.roomId)
        label.textColor = .greyColor
        label.textAlignment = .center
        return label
    }()
    
    private lazy var fansLabel: UILabel = {
        let label = UILabel()
        label.font = .customFont(ofSize: 12)
        label.textColor = .greyColor
        label.textAlignment = .center
        return label
    }()
    
    private let followButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = .b1
        button.setTitleColor(.flowKitWhite, for: .normal)
        button.layer.cornerRadius = 8.scale375Width()
        button.setTitle(.followText, for: .normal)
        button.titleLabel?.font = .customFont(ofSize: 14)
        return button
    }()
    
    init(service: RoomInfoService, enableFollow: Bool) {
        self.service = service
        self.state = service.state
        self.enableFollow = enableFollow
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
    
    func initialize(roomInfo: TUIRoomInfo) {
        service.initRoomInfo(roomInfo: roomInfo);
    }
    
    override func constructViewHierarchy() {
        addSubview(backgroundView)
        addSubview(titleLabel)
        addSubview(roomIdLabel)
        if enableFollow {
            addSubview(fansLabel)
        }
        if !isOwner && enableFollow {
            addSubview(followButton)
        }
        addSubview(imageView)
    }
    
    override func activateConstraints() {
        imageView.snp.makeConstraints { make in
            make.top.centerX.equalToSuperview()
            make.height.width.equalTo(55.scale375Width())
        }
        var totalHeight = isOwner ? 159 : 212
        if !enableFollow {
            totalHeight = 132
        }
        backgroundView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(29.scale375Height())
            make.leading.trailing.bottom.equalToSuperview()
            make.height.equalTo(totalHeight.scale375Height())
        }
        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(65.scale375Height())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }
        roomIdLabel.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(10.scale375Height())
            make.centerX.equalToSuperview()
            make.height.equalTo(17.scale375Height())
        }
        if enableFollow {
            fansLabel.snp.makeConstraints { make in
                make.top.equalTo(roomIdLabel.snp.bottom).offset(10.scale375Height())
                make.leading.trailing.equalToSuperview()
                make.height.equalTo(17.scale375Height())
            }
        }
        if !isOwner && enableFollow {
            followButton.snp.makeConstraints { make in
                make.top.equalTo(fansLabel.snp.bottom).offset(24.scale375Height())
                make.leading.equalToSuperview().offset(15.scale375Width())
                make.trailing.equalToSuperview().offset(-16.scale375Width())
                make.height.equalTo(40.scale375Height())
            }
        }
    }
    
    override func bindInteraction() {
        followButton.addTarget(self, action: #selector(followButtonClick(_:)), for: .touchUpInside)
        subscribeRoomInfoPanelState()
    }
    
    override func layoutSubviews() {
        super.layoutSubviews()
        updateFansView()
    }
    
    private func updateFansView() {
        service.getFansNumber()
    }
    
    @objc private func followButtonClick(_ sender: UIButton) {
        if state.followingList.contains(where: { $0.userId == state.ownerId }) {
            service.unfollowUser(userId: state.ownerId)
        } else {
            service.followUser(userId: state.ownerId)
        }
    }
    
    private func updateFollowButtonVisibility(visible: Bool) {
        if !enableFollow {
            return
        }
        if visible {
            addSubview(followButton)
            followButton.snp.makeConstraints { make in
                make.top.equalTo(fansLabel.snp.bottom).offset(24.scale375Height())
                make.leading.equalToSuperview().offset(15.scale375Width())
                make.trailing.equalToSuperview().offset(-16.scale375Width())
                make.height.equalTo(40.scale375Height())
            }
        } else {
            followButton.removeFromSuperview()
        }
    }
    
    private func subscribeRoomInfoPanelState() {
        state.$ownerAvatarUrl
            .receive(on: RunLoop.main)
            .sink { [weak self] avatarUrl in
                guard let self = self else { return }
                self.imageView.kf.setImage(with: URL(string: avatarUrl), placeholder: UIImage.placeholderImage)
            }
            .store(in: &cancellableSet)
        
        state.$ownerName
            .receive(on: RunLoop.main)
            .sink { [weak self] name in
                guard let self = self else { return }
                self.titleLabel.text = name
            }
            .store(in: &cancellableSet)
        
        state.$fansNumber
            .receive(on: RunLoop.main)
            .sink { [weak self] count in
                guard let self = self else { return }
                self.fansLabel.text = .localizedReplace(.fansCountText, replace: "\(count)")
            }
            .store(in: &cancellableSet)
        
        state.$followingList
            .receive(on: RunLoop.main)
            .sink { [weak self] userList in
                guard let self = self else { return }
                let userIdList = userList.map { $0.userId }
                if userIdList.contains(self.state.ownerId) {
                    self.followButton.isSelected = true
                    self.followButton.backgroundColor = .g3.withAlphaComponent(0.3)
                    self.followButton.setTitle(.unfollowText, for: .normal)
                } else {
                    self.followButton.isSelected = false
                    self.followButton.backgroundColor = .deepSeaBlueColor
                    self.followButton.setTitle(.followText, for: .normal)
                }
            }
            .store(in: &cancellableSet)
        
        state.$ownerId
            .receive(on: RunLoop.main)
            .sink { [weak self] ownerId in
                guard let self = self else { return }
                self.updateFollowButtonVisibility(visible: ownerId != self.state.selfUserId)
            }
            .store(in: &cancellableSet)
    }
}

// MARK: Action

fileprivate extension String {
    static let roomIdText = internalLocalized("Live Room ID: xxx")
    static let fansCountText = internalLocalized("xxx Fans")
    static let followText = internalLocalized("Follow")
    static let unfollowText = internalLocalized("Unfollow")
}
