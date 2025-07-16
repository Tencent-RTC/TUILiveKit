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
import RTCRoomEngine

public class LiveInfoView: UIView {
    private let service = RoomInfoService()
    public var state: RoomInfoState {
        service.state
    }
    public var isOwner: Bool {
        state.selfUserId == state.ownerId
    }
    private lazy var roomInfoPanelView = RoomInfoPanelView(service: service, enableFollow: enableFollow)
    private var cancellableSet = Set<AnyCancellable>()
    private let enableFollow: Bool
    private weak var popupViewController: UIViewController?
    
    private lazy var roomOwnerNameLabel: UILabel = {
        let view = UILabel()
        view.font = .customFont(ofSize: 14, weight: .semibold)
        view.textColor = .g8
        view.textAlignment = .left
        view.isUserInteractionEnabled = false
        return view
    }()

    private lazy var imageView: UIImageView = {
        let view = UIImageView(frame: CGRect(origin: .zero, size: CGSize(width: 32.scale375(), height: 32.scale375())))
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
        button.setImage(internalImage("live_user_followed_icon"), for: .selected)
        button.layer.cornerRadius = 12.scale375()
        return button
    }()
    
    public func initialize(roomInfo: TUIRoomInfo) {
        service.initRoomInfo(roomInfo: roomInfo)
    }
    
    public init(enableFollow: Bool = true, frame: CGRect = .zero) {
        self.enableFollow = enableFollow
        super.init(frame: frame)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    public override func didMoveToWindow() {
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
        if !isOwner && enableFollow {
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
        if !isOwner && enableFollow {
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
    
    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
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
                    self.followButton.setImage(internalImage("live_user_followed_icon"), for: .selected)
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
                self.imageView.kf.setImage(with: URL(string: avatarUrl), placeholder: avatarPlaceholderImage)
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
        state.roomDismissedSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] dismissedRoomId in
                guard let self = self, dismissedRoomId == state.roomId else { return }
                popupViewController?.dismiss(animated: true)
                popupViewController = nil
            }
            .store(in: &cancellableSet)
    }
    
    private func checkIsFollow(userId: String) {
        service.isFollow(userId: userId)
    }
    
    private func updateFollowButtonVisibility(visible: Bool) {
        if !enableFollow {
            return
        }
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
        if !WindowUtils.isPortrait { return }
        if let vc = WindowUtils.getCurrentWindowViewController() {
            popupViewController = vc
            let menuContainerView = MenuContainerView(contentView: roomInfoPanelView)
            menuContainerView.blackAreaClickClosure = { [weak self] in
                guard let self = self else { return }
                popupViewController?.dismiss(animated: true)
                popupViewController = nil
            }
            let viewController = PopupViewController(contentView: menuContainerView)
            popupViewController?.present(viewController, animated: true)
        }
    }
}


fileprivate extension String {
    static let followText = internalLocalized("Follow")
}
