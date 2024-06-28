//
//  TopView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//

import UIKit
import SnapKit
import Kingfisher
import Combine

private let containerHeight = 36.0
private let componentHeight = 32.0

protocol TopViewDelegate: AnyObject {
    func topView(_ topView: TopView, tap event:TopView.TapEvent, sender: Any?) -> Void
}

class UserAvatarCell: UICollectionViewCell {
    static let identifier: String = "UserAvatarCell"
    lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 24.scale375() * 0.5
        imageView.layer.masksToBounds = true
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

class TopView: UIView {
    
    enum TapEvent {
        case stop
        case audienceList
        case roomInfo
    }
    
    private var store: LiveStore
    private lazy var isOwnerPublisher = self.store.select(UserSelectors.isOwner)
    private lazy var userCount = self.store.select(RoomSelectors.getUserCount)
    private lazy var userList = self.store.select(UserSelectors.getUserList)

    var cancellableSet: Set<AnyCancellable> = []
    
    var memberAvatars: [String] = [] {
        didSet {
            collectionView.reloadData()
        }
    }
    
    weak var delegate: TopViewDelegate?
   
    private lazy var roomInfoView: RoomInfoView = {
        let view = RoomInfoView(store: store)
        view.mm_h = Int(componentHeight).scale375()
        view.backgroundColor = UIColor.g2.withAlphaComponent(0.4)
        view.layer.cornerRadius = view.mm_h * 0.5
        return view
    }()
    
    let audienceContainer: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .g2.withAlphaComponent(0.4)
        view.layer.cornerRadius = componentHeight/2.0
        return view
    }()
    
    lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .horizontal
        layout.itemSize = CGSize(width: 24.scale375(), height: 24.scale375())
        layout.minimumLineSpacing = -8.scale375()
        layout.minimumInteritemSpacing = 4.scale375()
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        collectionView.showsVerticalScrollIndicator = false
        collectionView.showsHorizontalScrollIndicator = false
        collectionView.backgroundColor = .clear
        collectionView.isUserInteractionEnabled = true
        collectionView.contentMode = .scaleToFill
        collectionView.dataSource = self
        collectionView.register(UserAvatarCell.self, forCellWithReuseIdentifier: UserAvatarCell.identifier)
        return collectionView
    }()
    
    let memberCountLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .whiteColor
        label.font = UIFont.systemFont(ofSize: 10)
        return label
    }()
    
    let stopButton: UIButton = {
        let button = UIButton(type: .system)
        button.setBackgroundImage(.liveBundleImage( "live_anchor_close_icon"), for: .normal)
        return button
    }()
    
    init(store: LiveStore) {
        self.store = store
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activeViewConstraint()
        bindInteraction()
        subscribeOwnerState()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(roomInfoView)
        addSubview(audienceContainer)
        audienceContainer.addSubview(collectionView)
        audienceContainer.addSubview(memberCountLabel)
        addSubview(stopButton)
    }
    
    private func activeViewConstraint() {
        roomInfoView.snp.remakeConstraints { make in
            make.centerY.equalTo(audienceContainer)
            make.height.equalTo(roomInfoView.mm_h)
            make.width.greaterThanOrEqualTo(80.scale375())
            make.width.lessThanOrEqualTo(375.scale375()*0.5)
            make.leading.equalToSuperview().inset(16.scale375())
            make.top.bottom.equalToSuperview()
        }
        audienceContainer.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.height.equalTo(componentHeight)
            make.width.lessThanOrEqualTo(74)
            make.right.equalTo(stopButton.snp.left).offset(-4)
        }
        collectionView.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(4)
            make.height.equalToSuperview()
            make.centerY.equalToSuperview()
            make.width.lessThanOrEqualTo(74)
        }
        memberCountLabel.snp.makeConstraints { make in
            make.right.equalToSuperview().offset(-4)
            make.height.equalToSuperview()
            make.centerY.equalToSuperview()
            make.left.equalTo(collectionView.snp.right).offset(4)
        }
        stopButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.right.equalToSuperview().offset(-16)
            make.height.width.equalTo(componentHeight)
        }
    }
    
    private func bindInteraction() {
        stopButton.addTarget(self, action: #selector(buttonTouchUpInSide(sender:)), for: .touchUpInside)
    
        let roomInfoViewTap = UITapGestureRecognizer(target: self, action: #selector(roomInfoViewTap(sender:)))
        roomInfoView.addGestureRecognizer(roomInfoViewTap)
        let audienceContainerTap = UITapGestureRecognizer(target: self, action: #selector(audienceContainerTap(sender:)))
        audienceContainer.addGestureRecognizer(audienceContainerTap)
    }
    
    private func subscribeOwnerState() {
        isOwnerPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] isOwner in
                guard let self = self else { return }
                self.stopButton.setBackgroundImage(.liveBundleImage( isOwner ? "live_anchor_close_icon" : "live_leave_icon"), for: .normal)
            }
            .store(in: &cancellableSet)
        userCount
            .receive(on: RunLoop.main)
            .sink{ [weak self] userCount in
                guard let self = self else { return }
                self.memberCountLabel.text = "\(userCount)"
            }
            .store(in: &cancellableSet)
        userList
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] userList in
                guard let self = self else { return }
                let avatarUrlList = userList.filter { [weak self] in
                    guard let self = self else { return true }
                    return $0.userId != self.store.selectCurrent(RoomSelectors.roomOwnerId)
                }.map { user in
                    return user.avatarUrl
                }
                memberAvatars = avatarUrlList
            })
            .store(in: &cancellableSet)
    }
}

extension TopView: UICollectionViewDataSource {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }
    
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return memberAvatars.count > 2 ? 2 : memberAvatars.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: UserAvatarCell.identifier, for: indexPath)
        if let avatarCell = cell as? UserAvatarCell {
            let avatar = memberAvatars[indexPath.item]
            if let url = URL(string: avatar) {
                avatarCell.avatarImageView.kf.setImage(with: url)
            }
        }
        return cell
    }
}

extension TopView {
    @objc
    private func buttonTouchUpInSide(sender: UIButton) {
        self.delegate?.topView(self, tap: .stop, sender: sender)
    }
    
    @objc func roomInfoViewTap(sender: UITapGestureRecognizer) {
        self.delegate?.topView(self, tap: .roomInfo, sender: sender)
    }
    
    @objc
    private func audienceContainerTap(sender: UITapGestureRecognizer) {
        self.delegate?.topView(self, tap: .audienceList, sender: sender)
    }
}
