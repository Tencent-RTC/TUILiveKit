//
//  AudienceListView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/20.
//

import Foundation
import RTCRoomEngine

class UserInfoCell: UICollectionViewCell {
    var userInfo: UserInfo? {
        didSet {
            if let url = URL(string: userInfo?.avatarUrl.value ?? "") {
                avatarImageView.kf.setImage(with: url, placeholder:UIImage.placeholderImage)
            } else {
                avatarImageView.image = .placeholderImage
            }
        }
    }

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

class AudienceListView: UIView {
    private let cellId = "TUIVideoSeatCell_Normal"
    private var listUser:[UserInfo] = []
    private var engineService: RoomEngineService
    private var liveRoomInfo:LiveRoomInfo {
          engineService.liveRoomInfo
    }
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        updateView()
        liveRoomInfo.audienceList.addObserver(self) { [weak self] _, _ in
            self?.updateView()
        }
        liveRoomInfo.audienceCount.addObserver(self) { [weak self] _, _ in
            self?.updateView()
        }
        updateView()
    }

    private lazy var containerView: UIView = {
        let view = UIView()
        view.backgroundColor = UIColor.g2.withAlphaComponent(0.4)
        view.layer.masksToBounds = true
        view.mm_w = 74.scale375()
        view.mm_h = 24.scale375Height()
        view.layer.cornerRadius = view.mm_h*0.5
        let tap = UITapGestureRecognizer(target: self, action: #selector(containerTapAction))
        view.addGestureRecognizer(tap)
        view.isUserInteractionEnabled = true
        return view
    }()
    
    private let numberLabel: UILabel = {
        let view = UILabel()
        view.textColor = .g7
        view.font = .customFont(ofSize: 10)
        view.mm_w = LocalizedLanguage.isChinese ? 28.scale375() : 56.scale375()
        view.mm_h = 20.scale375()
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
        collectionView.register(UserInfoCell.self, forCellWithReuseIdentifier: cellId)
        return collectionView
    }()

    private func updateView() {
        numberLabel.text = .localizedReplace(.audienceCountUnit, replace: "\(liveRoomInfo.audienceCount.value)")
        collectionView.snp.updateConstraints { make in
            make.width.equalTo(45.scale375())
        }
        
        listUser = liveRoomInfo.audienceList.value.filter({ $0.userId != self.liveRoomInfo.anchorInfo.value.userId })
        if engineService.liveKitStore.selfInfo.status.value == .none
            && engineService.liveKitStore.selfInfo.role.value == .anchor {
            listUser = listUser.filter({ $0.userId != engineService.liveKitStore.selfInfo.userId })
        }
        collectionView.reloadData()
    }
}

// MARK: Layout

extension AudienceListView {
    func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(containerView)
        containerView.addSubview(numberLabel)
        containerView.addSubview(collectionView)
    }

    func activateConstraints() {
        containerView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        numberLabel.snp.makeConstraints { make in
            make.trailing.equalToSuperview()
            make.centerY.equalToSuperview()
            make.width.equalTo(numberLabel.mm_w)
            make.height.equalTo(numberLabel.mm_h)
        }

        collectionView.snp.makeConstraints { make in
            make.trailing.equalTo(numberLabel.snp.leading)
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
            make.width.equalTo(38.scale375())
        }
    }
}

// MARK: - UICollectionViewDataSource

extension AudienceListView: UICollectionViewDataSource {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }

    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return listUser.count
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: cellId, for: indexPath) as! UserInfoCell
        if indexPath.item >= listUser.count {
            return cell
        }
        let userInfo = listUser[indexPath.item]
        cell.userInfo = userInfo
        return cell
    }
}

// MARK: Action

extension AudienceListView {
    @objc func containerTapAction() {
        PopupPanelController.alertView(RecentWatchMemberPanel(engineService: engineService))
    }
}

private extension String {
    static var audienceCountUnit: String {
        localized("live.audience.count.unit.xxx")
    }
}
