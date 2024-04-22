//
//  TopView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/13.
//

import UIKit
import SnapKit
import Kingfisher

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
    
    weak var delegate: TopViewDelegate?
    var roomName: String = "" {
        didSet {
            roomNameLabel.text = roomName
        }
    }
    var roomCoverUrl: URL? = nil {
        didSet {
            if let url = roomCoverUrl {
                roomCoverImageView.kf.setImage(with: url)
            }
        }
    }
    var memberCount: Int = 0 {
        didSet {
            memberCountLabel.text = "\(memberCount)"
        }
    }
    var memberAvatars: [String] = [] {
        didSet {
            collectionView.reloadData()
        }
    }
    private var isViewReady: Bool = false
    // private view
    let leftContainer: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .g2.withAlphaComponent(0.4)
        view.layer.cornerRadius = containerHeight/2.0
        return view
    }()
    
    let roomCoverImageView: UIImageView = {
        let view = UIImageView(frame: .zero)
        view.layer.cornerRadius = componentHeight/2.0
        view.layer.masksToBounds = true
        return view
    }()
    
    let roomNameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .whiteColor
        return label
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
        button.setBackgroundImage(.liveBundleImage("live_anchor_close_icon"), for: .normal)
        return button
    }()
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activeViewConstraint()
        bindInteraction()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(leftContainer)
        leftContainer.addSubview(roomCoverImageView)
        leftContainer.addSubview(roomNameLabel)
        addSubview(audienceContainer)
        audienceContainer.addSubview(collectionView)
        audienceContainer.addSubview(memberCountLabel)
        addSubview(stopButton)
    }
    
    private func activeViewConstraint() {
        leftContainer.snp.makeConstraints { make in
            make.top.bottom.equalToSuperview()
            make.height.equalTo(containerHeight)
            make.left.equalToSuperview().offset(16)
        }
        roomNameLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.right.equalToSuperview().offset(-12)
            make.left.equalTo(roomCoverImageView.snp.right).offset(4)
            make.height.equalTo(componentHeight)
        }
        roomCoverImageView.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.height.width.equalTo(componentHeight)
            make.left.equalToSuperview().offset(4)
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
        
        let tap = UITapGestureRecognizer(target: self, action: #selector(audienceContainerTap(sender:)))
        audienceContainer.addGestureRecognizer(tap)
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
    
    @objc
    private func audienceContainerTap(sender: UITapGestureRecognizer) {
        self.delegate?.topView(self, tap: .audienceList, sender: sender)
    }
}
