//
//  UserInfoPanelView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/2/24.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine
import ImSDK_Plus
import TUILiveResources

enum UserManagePanelType {
    case messageAndKickOut
    case mediaAndSeat
    case userInfo
}

class UserInfoPanelView: RTCBaseView {
    private let manager: LiveStreamManager
    
    @Published private var user: TUIUserInfo
    @Published private var isFollow = false
    @Published private var fansNumber = 0
    
    private var cancellableSet = Set<AnyCancellable>()
    
    private lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.layer.cornerRadius = 55.scale375() / 2
        imageView.layer.masksToBounds = true
        imageView.layer.borderWidth = 2.0
        imageView.layer.borderColor = UIColor.cyanColor.cgColor
        return imageView
    }()
    
    private let backgroundView : UIView = {
        let view = UIView()
        view.backgroundColor = .g2
        view.layer.cornerRadius = 12.scale375()
        view.layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        return view
    }()
    
    private lazy var userNameLabel: UILabel = {
        let label = UILabel()
        label.font = .customFont(ofSize: 16)
        label.text = user.userName
        label.textColor = .g7
        label.textAlignment = .center
        return label
    }()
    
    private lazy var userIdLabel: UILabel = {
        let label = UILabel()
        label.font = .customFont(ofSize: 12)
        label.text = "UserId: " + user.userId
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
        button.layer.cornerRadius = 20
        button.setTitle(.followText, for: .normal)
        button.titleLabel?.font = .customFont(ofSize: 14)
        return button
    }()
    
    init(user: TUIUserInfo, manager: LiveStreamManager) {
        self.user = user
        self.manager = manager
        super.init(frame: .zero)
        getUserInfo()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    override func constructViewHierarchy() {
        addSubview(backgroundView)
        addSubview(userNameLabel)
        addSubview(userIdLabel)
        addSubview(fansLabel)
        addSubview(followButton)
        addSubview(avatarImageView)
    }
    
    override func activateConstraints() {
        avatarImageView.snp.makeConstraints { make in
            make.top.centerX.equalToSuperview()
            make.height.width.equalTo(55.scale375Width())
        }
        backgroundView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(29.scale375Height())
            make.leading.trailing.bottom.equalToSuperview()
            make.height.equalTo(212.scale375Height())
        }
        userNameLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(65.scale375Height())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }
        userIdLabel.snp.makeConstraints { make in
            make.top.equalTo(userNameLabel.snp.bottom).offset(10.scale375Height())
            make.centerX.equalToSuperview()
            make.height.equalTo(17.scale375Height())
        }
        fansLabel.snp.makeConstraints { make in
            make.top.equalTo(userIdLabel.snp.bottom).offset(10.scale375Height())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(17.scale375Height())
        }
        followButton.snp.makeConstraints { make in
            make.top.equalTo(fansLabel.snp.bottom).offset(24.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalTo(275.scale375())
            make.height.equalTo(40.scale375Height())
        }
    }
    
    override func bindInteraction() {
        followButton.addTarget(self, action: #selector(followButtonClick), for: .touchUpInside)
        subscribeRoomInfoPanelState()
    }
    
    override func setupViewStyle() {
        avatarImageView.kf.setImage(with: URL(string: user.avatarUrl), placeholder: UIImage.avatarPlaceholderImage)
        initFansView()
        checkFollowType()
    }
    
    private func getUserInfo() {
        Task {
            do {
                let info = try await manager.getUserInfo(userId: user.userId)
                user = info
            } catch let err as InternalError {
                debugPrint("getUserInfoError: \(err.localizedMessage)")
            }
        }
    }
    
    private func initFansView() {
        V2TIMManager.sharedInstance().getUserFollowInfo(userIDList: [user.userId]) { [weak self] followInfoList in
            guard let self = self, let followInfo = followInfoList?.first else { return }
            fansNumber = Int(followInfo.followersCount)
        } fail: { code, message in
            debugPrint("getFansNumber failed, error:\(code), message:\(String(describing: message))")
        }
    }
    
    private func checkFollowType() {
        V2TIMManager.sharedInstance().checkFollowType(userIDList: [user.userId]) { [weak self] checkResultList in
            guard let self = self, let result = checkResultList?.first else { return }
            if result.followType == .FOLLOW_TYPE_IN_BOTH_FOLLOWERS_LIST || result.followType == .FOLLOW_TYPE_IN_MY_FOLLOWING_LIST {
                self.isFollow = true
            } else {
                self.isFollow = false
            }
        } fail: { code, message in
            debugPrint("checkFollowType failed, error:\(code), message:\(String(describing: message))")
        }
    }
    
    private func subscribeRoomInfoPanelState() {
        $fansNumber
            .receive(on: RunLoop.main)
            .sink { [weak self] count in
                guard let self = self else { return }
                self.fansLabel.text = .localizedReplace(.fansCountText, replace: "\(count)")
            }
            .store(in: &cancellableSet)
        
        $isFollow.receive(on: RunLoop.main)
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] isFollow in
                guard let self = self else { return }
                if isFollow {
                    followButton.backgroundColor = .g5
                    followButton.setTitle(.unfollowText, for: .normal)
                } else {
                    followButton.backgroundColor = .deepSeaBlueColor
                    followButton.setTitle(.followText, for: .normal)
                }
            }
            .store(in: &cancellableSet)
        
        $user.receive(on: RunLoop.main)
            .sink { [weak self] user in
                guard let self = self else { return }
                userNameLabel.text = user.userName.isEmpty ? user.userId : user.userName
                avatarImageView.kf.setImage(with: URL(string: user.avatarUrl), placeholder: UIImage.avatarPlaceholderImage)
            }
            .store(in: &cancellableSet)
    }
}

// MARK: - Action
extension UserInfoPanelView {
    @objc private func followButtonClick() {
        if isFollow {
            V2TIMManager.sharedInstance().unfollowUser(userIDList: [user.userId]) { [weak self] followResultList in
                guard let self = self, let result = followResultList?.first else { return }
                if result.resultCode == 0 {
                    isFollow = false
                    fansNumber -= 1
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
                    fansNumber += 1
                } else {
                    manager.toastSubject.send("code: \(result.resultCode), message: \(String(describing: result.resultInfo))")
                }
            } fail: { [weak self] code, message in
                guard let self = self else { return }
                manager.toastSubject.send("code: \(code), message: \(String(describing: message))")
            }
        }
    }
}

fileprivate extension String {
    static let fansCountText = internalLocalized("xxx Fans")
    static let followText = internalLocalized("Follow")
    static let unfollowText = internalLocalized("Unfollow")
    }
