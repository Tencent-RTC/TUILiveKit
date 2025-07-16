//
//  LinkMicAudienceFloatView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/2.
//

import UIKit
import TUICore
import Combine
import RTCCommon
import RTCRoomEngine
import LiveStreamCore

class UserImageCell: UICollectionViewCell {
    var user: TUIUserInfo? {
        didSet {
            if let url = URL(string: user?.avatarUrl ?? "") {
                avatarImageView.kf.setImage(with: url, placeholder: UIImage.avatarPlaceholderImage)
            } else {
                avatarImageView.image = .avatarPlaceholderImage
            }
        }
    }

    lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 40.scale375() * 0.5
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
    
    func setImage(image: UIImage?) {
        avatarImageView.kf.cancelDownloadTask()
        avatarImageView.image = image
    }
}

protocol LinkMicAudienceFloatViewDelegate: AnyObject {
    func cancelApplication()
}

class LinkMicAudienceFloatView: UIView {
    weak var delegate: LinkMicAudienceFloatViewDelegate?
    
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
    
    private var cancellableSet = Set<AnyCancellable>()
    
    private var dotsTimer: Timer = Timer()
    private var isViewReady: Bool = false
    init(manager: AudienceManager, routerManager: AudienceRouterManager) {
        self.manager = manager
        self.routerManager = routerManager
        super.init(frame: .zero)
        updateLabelText()
        subscribeViewState()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private func subscribeViewState() {
        manager.subscribeState(StateSelector(keyPath: \AudienceCoGuestState.coGuestStatus))
            .receive(on: RunLoop.main)
            .sink { [weak self] status in
                self?.isHidden = status != .applying
            }
            .store(in: &cancellableSet)
    }
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .black
        constructViewHierarchy()
        activateConstraints()
        updateView()
        manager.subscribeState(StateSelector(keyPath: \AudienceUserState.userList))
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.updateView()
            }
            .store(in: &cancellableSet)
    }

    let tipsLabel: UILabel = {
        let label = UILabel()
        label.textColor = .flowKitWhite
        label.text = .localizedReplace(.toBePassedText, replace: "")
        label.font = .customFont(ofSize: 14)
        label.sizeToFit()
        label.adjustsFontSizeToFitWidth = true
        if TUIGlobalization.getRTLOption() {
            label.textAlignment = .right
        } else {
            label.textAlignment = .left
        }
        return label
    }()

    lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .horizontal
        layout.itemSize = CGSize(width: 40.scale375(), height: 40.scale375())
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        collectionView.showsVerticalScrollIndicator = false
        collectionView.showsHorizontalScrollIndicator = false
        collectionView.backgroundColor = .clear
        collectionView.isUserInteractionEnabled = true
        collectionView.contentMode = .scaleToFill
        collectionView.dataSource = self
        collectionView.register(UserImageCell.self, forCellWithReuseIdentifier: UserImageCell.cellReuseIdentifier)
        let tap = UITapGestureRecognizer(target: self, action: #selector(tapAction))
        collectionView.addGestureRecognizer(tap)
        return collectionView
    }()

    private func updateView() {
        collectionView.reloadData()
    }

    private func updateLabelText() {
        var dots = ""
        dotsTimer = Timer(timeInterval: 1.0, repeats: true) { [weak self] _ in
            guard let self = self else { return }
            if dots.count == 3 {
                dots.removeAll()
            }else {
                dots.append(".")
            }
            self.tipsLabel.text? = .localizedReplace(.toBePassedText, replace: dots)
        }
        RunLoop.current.add(dotsTimer, forMode: .default)
    }

    deinit {
        dotsTimer.invalidate()
    }
}

// MARK: Layout

extension LinkMicAudienceFloatView {
    func constructViewHierarchy() {
        backgroundColor = .g2.withAlphaComponent(0.4)
        layer.cornerRadius = 10
        layer.masksToBounds = true
        layer.borderWidth = 1
        layer.borderColor = UIColor.flowKitWhite.withAlphaComponent(0.2).cgColor
        
        addSubview(tipsLabel)
        addSubview(collectionView)
    }

    func activateConstraints() {
        collectionView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(14.scale375Width())
            make.height.width.equalTo(40.scale375())
            make.centerX.equalToSuperview()
        }

        tipsLabel.snp.makeConstraints { make in
            make.top.equalTo(collectionView.snp.bottom).offset(6.scale375Width())
            make.centerX.equalToSuperview()
            make.width.equalTo(55.scale375())
            make.height.equalTo(20.scale375())
        }
    }
}

// MARK: - UICollectionViewDataSource

extension LinkMicAudienceFloatView: UICollectionViewDataSource {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }

    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return 1
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: UserImageCell.cellReuseIdentifier,
                                                      for: indexPath) as! UserImageCell
        cell.user = manager.coreUserState.selfInfo
        return cell
    }
}

// MARK: - UICollectionViewDelegate

extension LinkMicAudienceFloatView: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout:
                        UICollectionViewLayout, insetForSectionAt section: Int) -> UIEdgeInsets {
        let width = collectionView.frame.width
        let margin = width * 0.3
        return UIEdgeInsets(top: 10, left: margin / 2, bottom: 10, right: margin / 2)
    }
}

// MARK: Action

extension LinkMicAudienceFloatView {
    @objc func tapAction() {
        showCancelLinkMicPanel()
    }
    
    private func showCancelLinkMicPanel() {
        var items: [ActionItem] = []
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        let item = ActionItem(title: .cancelLinkMicRequestText, designConfig: designConfig) { [weak self] _ in
            guard let self = self else { return }
            delegate?.cancelApplication()
            self.routerManager.router(action: .dismiss())
        }
        items.append(item)
        routerManager.router(action: .present(.listMenu(ActionPanelData(items: items, cancelText: .cancelText))))
    }
}

private extension String {
    static var toBePassedText: String {
        internalLocalized("Waitingxxx")
    }
    static var cancelLinkMicRequestText = {
        internalLocalized("Cancel application for link mic")
    }()
    static var cancelText = {
        internalLocalized("Cancel")
    }()
}
