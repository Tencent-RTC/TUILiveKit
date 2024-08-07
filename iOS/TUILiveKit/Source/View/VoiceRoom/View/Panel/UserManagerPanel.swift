//
//  UserManagerPanel.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/7/31.
//

import UIKit
import RTCCommon
import Combine
import RTCRoomEngine

class UserManagerPanel: RTCBaseView {
    private let store: LiveStore
    private let routerStore: RouterStore
    private var cancellableSet: Set<AnyCancellable> = []
    private var seatInfo: SeatInfo
    private lazy var isOwner: Bool = store.selectCurrent(UserSelectors.isOwner)
    
    private let avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 20.scale375()
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    private let userContentView: UIView = {
        let view = UIView(frame: .zero)
        return view
    }()
    
    private let userNameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .grayColor
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.adjustsFontSizeToFitWidth = true
        return label
    }()
    
    private let userIdLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .greyColor
        label.font = .customFont(ofSize: 12, weight: .regular)
        return label
    }()
    
    private let levelButton: UIButton = {
        let button = UIButton()
        button.layer.cornerRadius = 7.scale375Height()
        button.titleLabel?.textColor = .flowKitWhite
        button.isEnabled = false
        let spacing: CGFloat = 2.scale375()
        button.imageEdgeInsets = UIEdgeInsets(top: 0, left: -spacing / 2, bottom: 0, right: spacing / 2)
        button.titleEdgeInsets = UIEdgeInsets(top: 0, left: spacing / 2, bottom: 0, right: -spacing / 2)
        button.titleLabel?.font = UIFont(name: "PingFangSC-Regular", size: 12)
        return button
    }()
    
    private let followButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = .b1
        button.titleLabel?.font = .customFont(ofSize: 14, weight: .medium)
        button.setTitle(.followText, for: .normal)
        button.setTitleColor(.white, for: .normal)
        button.setImage(.liveBundleImage("live_user_followed_icon"), for: .selected)
        button.layer.cornerRadius = 16.scale375Height()
        button.isHidden = true
        return button
    }()
    
    private lazy var featureClickPanel: FeatureClickPanel = {
        var designConfig = FeatureItemDesignConfig()
        designConfig.backgroundColor = .g3.withAlphaComponent(0.3)
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.type = .imageAboveTitleBottom
        
        let model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 50.scale375())
        model.itemDiff = 25.scale375()
        model.items.append(FeatureItem(normalTitle: .muteText,
                                       normalImage: .liveBundleImage("live_anchor_mute_icon"),
                                       selectedTitle: .unmuteText,
                                       selectedImage: .liveBundleImage("live_anchor_unmute_icon"),
                                       isSelected: seatInfo.isAudioLocked,
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] button in
            guard let self = self else { return }
            self.muteClick(sender: button)
        }))
        model.items.append(FeatureItem(normalTitle: .kickoffText,
                                       normalImage: .liveBundleImage("live_anchor_kickoff_icon"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] button in
            guard let self = self else { return }
            self.kickoffClick()
        }))
        let featureClickPanel = FeatureClickPanel(model: model)
        featureClickPanel.isHidden = !isOwner
        return featureClickPanel
    }()
    
    init(store: LiveStore, routerStore: RouterStore, seatInfo: SeatInfo) {
        self.store = store
        self.routerStore = routerStore
        self.seatInfo = seatInfo
        super.init(frame: .zero)
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
    
    override func constructViewHierarchy() {
        addSubview(avatarImageView)
        addSubview(userContentView)
        userContentView.addSubview(userNameLabel)
        userContentView.addSubview(levelButton)
        userContentView.addSubview(userIdLabel)
        addSubview(followButton)
        addSubview(featureClickPanel)
    }
    
    override func activateConstraints() {
        avatarImageView.snp.makeConstraints { make in
            make.leading.top.equalToSuperview().offset(24.scale375())
            make.size.equalTo(CGSize(width: 40.scale375(), height: 40.scale375()))
        }
        
        userContentView.snp.makeConstraints { make in
            make.leading.equalTo(avatarImageView.snp.trailing).offset(12.scale375())
            make.top.bottom.equalTo(avatarImageView)
        }
        
        userNameLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview()
            make.top.equalToSuperview()
            make.width.lessThanOrEqualTo(150.scale375())
        }
        
        levelButton.snp.makeConstraints { make in
            make.leading.equalTo(userNameLabel.snp.trailing).offset(4.scale375())
            make.trailing.equalToSuperview()
            make.centerY.equalTo(userNameLabel.snp.centerY)
            make.size.equalTo(CGSize(width: 30.scale375(), height: 14.scale375Height()))
        }
        
        userIdLabel.snp.makeConstraints { make in
            make.leading.bottom.equalToSuperview()
            make.width.lessThanOrEqualTo(150.scale375())
        }
        
        followButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24.scale375())
            make.centerY.equalTo(userContentView.snp.centerY)
            make.size.equalTo(CGSizeMake(70.scale375(), 32.scale375Height()))
        }
        
        featureClickPanel.snp.makeConstraints { make in
            make.top.equalTo(avatarImageView.snp.bottom).offset(30.scale375Height())
            make.leading.equalTo(avatarImageView.snp.leading)
            make.bottom.equalToSuperview().offset(-24.scale375())
        }
    }
    
    override func bindInteraction() {
        subscribeMyFollowListState()
        subscribeSeatInfoState()
        followButton.addTarget(self, action:#selector(followButtonClick(sender:)), for: .touchUpInside)
    }
    
    override func setupViewStyle() {
        avatarImageView.kf.setImage(with: URL(string: seatInfo.avatarUrl), placeholder: UIImage.avatarPlaceholderImage)
        userNameLabel.text = seatInfo.userName
        userIdLabel.text = "ID: " + seatInfo.userId
        levelButton.setLevel()
    }
    
}

extension UserManagerPanel {
    private func subscribeMyFollowListState() {
        store.select(UserSelectors.getMyFollowingList)
            .receive(on: RunLoop.main)
            .sink { [weak self] followUserList in
                guard let self = self else { return }
                self.followButton.isHidden = false
                if followUserList.map({ $0.userId }).contains(where: { [weak self] in
                    guard let self = self else { return false }
                    return $0 == self.seatInfo.userId
                }) {
                    self.followButton.isSelected = true
                    self.followButton.backgroundColor = .g3.withAlphaComponent(0.3)
                    self.followButton.setTitle("", for: .normal)
                } else {
                    self.followButton.isSelected = false
                    self.followButton.backgroundColor = .deepSeaBlueColor
                    self.followButton.setTitle(.followText, for: .normal)
                }
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeSeatInfoState() {
        store.select(SeatSelectors.getSeatInfo(index: seatInfo.index))
            .receive(on: RunLoop.main)
            .sink { [weak self] seatInfo in
                guard let self = self else { return }
                self.seatInfo = seatInfo
            }
            .store(in: &cancellableSet)
    }
}

extension UserManagerPanel {
    @objc
    private func followButtonClick(sender: UIButton) {
        let user = User(seatInfo: seatInfo)
        store.dispatch(action: sender.isSelected ? UserActions.unfollow(payload: user) : UserActions.follow(payload: user))
    }
    
    @objc
    private func muteClick(sender: FeatureItemButton) {
        seatInfo.isAudioLocked = !seatInfo.isAudioLocked
        sender.isSelected = seatInfo.isAudioLocked
        
        let lockSeat = TUISeatLockParams()
        lockSeat.lockAudio = seatInfo.isAudioLocked
        lockSeat.lockVideo = seatInfo.isVideoLocked
        lockSeat.lockSeat = seatInfo.isLocked
        store.dispatch(action: SeatActions.lockSeat(payload: (seatInfo.index, lockSeat)))
    }
    
    @objc
    private func kickoffClick() {
        store.dispatch(action: SeatActions.kickSeat(payload: seatInfo))
        routerStore.router(action: .dismiss())
    }
}

fileprivate extension String {
    static let followText = localized("live.user.follow")
    static let muteText = localized("live.seat.mute")
    static let unmuteText = localized("live.seat.unmute")
    static let kickoffText = localized("live.anchor.link.hang.up.title")
}
