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

class UserInfoCell: UICollectionViewCell {
    var userInfo: User? {
        didSet {
            if let url = URL(string: userInfo?.avatarUrl ?? "") {
                avatarImageView.kf.setImage(with: url, placeholder:UIImage.avatarPlaceholderImage)
            } else {
                avatarImageView.image = .avatarPlaceholderImage
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

class AudienceListView: RTCBaseView {
    private let cellId = "TUIVideoSeatCell_Normal"
    
    // MARK: - private property.
    private let store: LiveStore
    private let routerStore: RouterStore
    private var cancellableSet: Set<AnyCancellable> = []
    private var listUser:[User] = []
    
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
        collectionView.register(UserInfoCell.self, forCellWithReuseIdentifier: cellId)
        return collectionView
    }()
    
    private lazy var numberBackgroundView: UIView = {
        let view = UIView()
        view.backgroundColor = UIColor.g1.withAlphaComponent(0.4)
        view.layer.cornerRadius = 10.scale375Height()
        return view
    }()
    
    private let numberLabel: UILabel = {
        let label = UILabel()
        label.textColor = .g7
        label.font = .customFont(ofSize: 10)
        label.textAlignment = .center
        return label
    }()
    
    private let imageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = .liveBundleImage("live_selection_arrow_icon")
        return imageView
    }()
    
    init(store: LiveStore, routerStore: RouterStore) {
        self.store = store
        self.routerStore = routerStore
        super.init(frame: .zero)
    }

    override func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(collectionView)
        addSubview(numberBackgroundView)
        numberBackgroundView.addSubview(numberLabel)
        numberBackgroundView.addSubview(imageView)
    }
    
    override func activateConstraints() {
        collectionView.snp.makeConstraints { make in
            make.leading.centerY.height.equalToSuperview()
            make.width.equalTo(84.scale375())
        }
        numberBackgroundView.snp.makeConstraints { make in
            make.leading.equalTo(collectionView.snp.trailing).offset(4.scale375())
            make.centerY.height.equalTo(collectionView)
            make.trailing.equalToSuperview()
        }
        numberLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(4.scale375())
            make.centerY.height.equalToSuperview()
            make.trailing.equalTo(imageView.snp.leading)
        }
        imageView.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-4.scale375())
            make.centerY.equalToSuperview()
            make.width.height.equalTo(8.scale375())
        }
    }
    
    override func setupViewStyle() {
        backgroundColor = .clear
    }
    
    override func bindInteraction() {
        store.select(RoomSelectors.getUserCount)
            .receive(on: RunLoop.main)
            .sink { [weak self] count in
                guard let self = self else { return }
                self.numberLabel.text = "\(count)"
                self.collectionView.reloadData()
            }
            .store(in: &cancellableSet)
        store.select(UserSelectors.getUserList)
            .receive(on: RunLoop.main)
            .sink { [weak self] userList in
                guard let self = self else { return }
                self.listUser = userList.filter { [weak self] in
                    guard let self = self else { return true }
                    return $0.userId != self.store.selectCurrent(RoomSelectors.roomOwnerId)
                }
                self.collectionView.reloadData()
            }
            .store(in: &cancellableSet)
        let tap = UITapGestureRecognizer(target: self, action: #selector(containerTapAction))
        addGestureRecognizer(tap)
        isUserInteractionEnabled = true
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

extension AudienceListView {
    @objc func containerTapAction() {
        routerStore.router(action: .present(.recentViewer))
    }
}
