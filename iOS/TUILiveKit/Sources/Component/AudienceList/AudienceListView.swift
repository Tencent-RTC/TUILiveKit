//
//  AudienceListView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/20.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine
import Kingfisher
import SnapKit

class UserInfoCell: UICollectionViewCell {
    var userInfo: TUIUserInfo? {
        didSet {
            if let url = URL(string: userInfo?.avatarUrl ?? "") {
                avatarImageView.kf.setImage(with: url, placeholder: avatarPlaceholderImage)
            } else {
                avatarImageView.image = avatarPlaceholderImage
            }
        }
    }

    lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 24.scale375() * 0.5
        imageView.layer.masksToBounds = true
        imageView.layer.borderWidth = 1
        imageView.layer.borderColor = UIColor.g1.withAlphaComponent(0.4).cgColor
        contentView.addSubview(imageView)
        return imageView
    }()

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        contentView.backgroundColor = .clear
        avatarImageView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

public class AudienceListView: RTCBaseView {
    public var onUserManageButtonClicked: ((TUIUserInfo) -> Void)?
    private let cellId = "TUIVideoSeatCell_Normal"
    
    // Set this value to change max display count
    private let displayAvatarCount = 2
    
    // MARK: - private property.
    private let service: AudienceListService = AudienceListService()
    private var state: AudienceListState {
        service.state
    }
    private lazy var observer: AudienceListObserver = AudienceListObserver(state: state, service: service)
    private var listUser: [TUIUserInfo] = []
    private var cancellableSet: Set<AnyCancellable> = []
    private weak var popupViewController: UIViewController?
    
    lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .horizontal
        layout.itemSize = CGSize(width: 24.scale375(), height: 24.scale375())
        layout.minimumLineSpacing = 4.scale375()
        layout.minimumInteritemSpacing = 4.scale375()
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        collectionView.showsVerticalScrollIndicator = false
        collectionView.showsHorizontalScrollIndicator = false
        collectionView.backgroundColor = .clear
        collectionView.semanticContentAttribute = .forceRightToLeft
        collectionView.isUserInteractionEnabled = true
        collectionView.contentMode = .scaleToFill
        collectionView.dataSource = self
        collectionView.isScrollEnabled = false
        collectionView.register(UserInfoCell.self, forCellWithReuseIdentifier: cellId)
        return collectionView
    }()
    
    private lazy var numberBackgroundView: UIView = {
        let view = UIView()
        view.backgroundColor = UIColor.g1.withAlphaComponent(0.4)
        view.layer.cornerRadius = 12.scale375()
        return view
    }()
    
    private let numberLabel: UILabel = {
        let label = UILabel()
        label.textColor = .g7
        label.font = .customFont(ofSize: 10)
        label.textAlignment = .center
        return label
    }()

    public func initialize(roomInfo: TUIRoomInfo) {
        TUIRoomEngine.sharedInstance().addObserver(observer)
        service.initRoomInfo(roomInfo: roomInfo)
    }
    
    public override func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(collectionView)
        addSubview(numberBackgroundView)
        numberBackgroundView.addSubview(numberLabel)
    }
    
    public override func activateConstraints() {
        collectionView.snp.makeConstraints { make in
            make.leading.top.bottom.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo((displayAvatarCount * 24 + (displayAvatarCount - 1) * 4).scale375())
        }
        numberBackgroundView.snp.makeConstraints { make in
            make.leading.equalTo(collectionView.snp.trailing).offset(4.scale375())
            make.centerY.height.equalTo(collectionView)
            make.width.greaterThanOrEqualTo(24.scale375())
            make.trailing.equalToSuperview()
        }
        let numberMaxWidth = getMaxWidthFromLabel(numberLabel, maxText: "999999")
        let numberMinWidth = getMaxWidthFromLabel(numberLabel, maxText: "0")
        numberLabel.snp.makeConstraints { make in
            make.centerY.centerX.height.equalToSuperview()
            make.width.lessThanOrEqualTo(numberMaxWidth)
            make.width.greaterThanOrEqualTo(numberMinWidth)
        }
    }
    
    private func getMaxWidthFromLabel(_ label: UILabel, maxText: String) -> CGFloat {
        let tmpLabel = UILabel(frame: .zero)
        tmpLabel.numberOfLines = 1
        tmpLabel.font = label.font
        tmpLabel.text = maxText
        tmpLabel.sizeToFit()
        return tmpLabel.frame.size.width
    }
    
    public override func setupViewStyle() {
        backgroundColor = .clear
    }
    
    public override func bindInteraction() {
        state.$audienceList
            .combineLatest(state.$audienceCount)
            .receive(on: RunLoop.main)
            .sink { [weak self] audienceList, audienceCount in
                guard let self = self else { return }
                self.listUser = audienceList
                self.collectionView.reloadData()
                
                let count = audienceCount > kMaxShowUserCount ? audienceCount : audienceList.count
                self.updateUserCount(count: count)
            }
            .store(in: &cancellableSet)
        
        state.roomDismissedSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] dismissedRoomId in
                guard let self = self, dismissedRoomId == state.roomId else { return }
                popupViewController?.dismiss(animated: true)
                popupViewController = nil
            }.store(in: &cancellableSet)
        
        let tap = UITapGestureRecognizer(target: self, action: #selector(containerTapAction))
        addGestureRecognizer(tap)
        isUserInteractionEnabled = true
    }
    
    deinit {
        TUIRoomEngine.sharedInstance().removeObserver(observer)
    }
    
    private func updateUserCount(count: Int) {
        numberLabel.text = "\(count)"
        numberLabel.sizeToFit()
        let maxWidth = getMaxWidthFromLabel(numberLabel, maxText: "999999")
        let currentWidth = numberLabel.frame.size.width
        numberLabel.snp.remakeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(4.scale375())
            make.centerY.height.equalToSuperview()
            make.width.equalTo(min(maxWidth, currentWidth))
        }
        superview?.layoutIfNeeded()
    }
}

// MARK: - UICollectionViewDataSource
extension AudienceListView: UICollectionViewDataSource {
    public func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }

    public func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return min(2, listUser.count)
    }

    public func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: cellId, for: indexPath) as! UserInfoCell
        if indexPath.item >= listUser.count {
            return cell
        }
        let userInfo = listUser[indexPath.item]
        cell.userInfo = userInfo
        return cell
    }
}

extension AudienceListView {
    @objc func containerTapAction() {
        if !WindowUtils.isPortrait { return }
        service.getUserList()
        if let vc = WindowUtils.getCurrentWindowViewController() {
            let audienceListPanel = AudienceListPanelView(state: state)
            audienceListPanel.onUserManageButtonClicked = onUserManageButtonClicked
            popupViewController = vc
            let menuContainerView = MenuContainerView(contentView: audienceListPanel)
            audienceListPanel.onBackButtonClickedClosure = { [weak self] in
                guard let self = self else { return }
                popupViewController?.dismiss(animated: true)
                popupViewController = nil
            }
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
