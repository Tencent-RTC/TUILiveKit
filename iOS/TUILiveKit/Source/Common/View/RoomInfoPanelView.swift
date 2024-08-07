//
//  RoomInfoPanelView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/5/22.
//

import Foundation
import Combine
import RTCCommon

class RoomInfoPanelView: RTCBaseView {

    var store: LiveStore
   
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var ownerInfoPublisher = self.store.select(RoomSelectors.getRoomOwnerInfo)
    private lazy var roomIdPublisher = self.store.select(RoomSelectors.getRoomId)
    private lazy var fansCountPublisher = self.store.select(RoomSelectors.getRoomOwnerFansCount)
    private lazy var followingUserListPublisher = self.store.select(UserSelectors.getMyFollowingList)

    private lazy var imageView: UIImageView = {
        let imageView = UIImageView()
        imageView.layer.cornerRadius = 55.scale375()/2
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    private let backgroundView : UIView = {
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
        label.textColor = .greyColor
        label.textAlignment = .center
        return label
    }()
    
    lazy var copyButton: UIButton = {
        let button = UIButton(type: .custom)
        button.setImage(.liveBundleImage("live_copy_icon"), for: .normal)
        button.setTitle(.copyTitle, for: .normal)
        button.titleLabel?.font = .customFont(ofSize: 10)
        
        button.titleLabel?.tintColor = .g5
        button.backgroundColor = .g4
        button.layer.cornerRadius = 6
        button.layer.masksToBounds = true
        button.imageEdgeInsets = UIEdgeInsets(top: 0, left: -10, bottom: 0, right: 0)
        button.titleEdgeInsets = UIEdgeInsets(top: 0, left: 5, bottom: 0, right: 0)
        button.addTarget(self, action: #selector(copyButtonClick), for: .touchUpInside)
        return button
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
    
    init(store: LiveStore) {
        self.store = store
        super.init(frame: .zero)
        store.dispatch(action: RoomActions.fetchRoomOwnerFansCount(payload: store.selectCurrent(RoomSelectors.roomOwnerId)))
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
    
    override func constructViewHierarchy() {
        addSubview(backgroundView)
        addSubview(titleLabel)
        addSubview(roomIdLabel)
        addSubview(copyButton)
        addSubview(fansLabel)
        if !store.selectCurrent(UserSelectors.isOwner) {
            addSubview(followButton)
        }
        addSubview(imageView)
    }
    
    override func activateConstraints() {
        imageView.snp.makeConstraints { make in
            make.top.centerX.equalToSuperview()
            make.height.width.equalTo(55.scale375Width())
        }
        backgroundView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(29.scale375Height())
            make.leading.trailing.bottom.equalToSuperview()
            make.height.equalTo(isOwner ? 159.scale375Height() : 212.scale375Height())
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
        copyButton.snp.makeConstraints { make in
            make.centerY.equalTo(roomIdLabel)
            make.leading.equalTo(roomIdLabel.snp.trailing).offset(16)
            make.width.equalTo(64)
            make.height.equalTo(22)
        }
        fansLabel.snp.makeConstraints { make in
            make.top.equalTo(roomIdLabel.snp.bottom).offset(10.scale375Height())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(17.scale375Height())
        }
        if !store.selectCurrent(UserSelectors.isOwner) {
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
    
    @objc func followButtonClick(_ sender: UIButton) {
        let roomOwnerInfo = store.selectCurrent(RoomSelectors.getRoomOwnerInfo)
        store.dispatch(action: sender.isSelected ? UserActions.unfollow(payload: roomOwnerInfo) : UserActions.follow(payload: roomOwnerInfo))
    }
    
    @objc func copyButtonClick() {
        UIPasteboard.general.string = store.roomState.roomId
        store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .copySuccessToast,position: .center)))
    }
    
    
    func subscribeRoomInfoPanelState() {
        ownerInfoPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] info in
                guard let self = self else { return }
                self.imageView.kf.setImage(with: URL(string: info.avatarUrl), placeholder: UIImage.placeholderImage)
                self.titleLabel.text = info.name
            }
            .store(in: &cancellableSet)
        roomIdPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] id in
                guard let self = self else { return }
                self.roomIdLabel.text = .localizedReplace(.roomIdText, replace: id)
            }
            .store(in: &cancellableSet)
        fansCountPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] count in
                guard let self = self else { return }
                self.fansLabel.text = .localizedReplace(.fansCountText, replace: "\(count)")
            }
            .store(in: &cancellableSet)
        followingUserListPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] userList in
                guard let self = self else { return }
                let userIdList = userList.map { $0.userId }
                if userIdList.contains(self.store.selectCurrent(RoomSelectors.roomOwnerId)) {
                    self.followButton.isSelected = true
                    self.followButton.backgroundColor = .g3.withAlphaComponent(0.3)
                    self.followButton.setTitle(.unfollowText, for: .normal)
                } else {
                    self.followButton.isSelected = false
                    self.followButton.backgroundColor = .deepSeaBlueColor
                    self.followButton.setTitle(.followText, for: .normal)
                }
                let roomOwnerId = self.store.selectCurrent(RoomSelectors.roomOwnerId)
                self.store.dispatch(action: RoomActions.fetchRoomOwnerFansCount(payload: roomOwnerId))
            }
            .store(in: &cancellableSet)
    }
}

extension RoomInfoPanelView {
    var isOwner: Bool {
        return store.selectCurrent(UserSelectors.isOwner)
    }
}

fileprivate extension String {
    static let roomIdText = localized("live.roomId.xxx")
    static let fansCountText = localized("live.fans.count.xxx")
    static let followText = localized("live.user.follow")
    static let unfollowText = localized("live.user.unfollow")
    static let copyTitle = localized("live.room.info.copy.title")
    static let copySuccessToast = localized("live.room.info.copy.success.title")
}
