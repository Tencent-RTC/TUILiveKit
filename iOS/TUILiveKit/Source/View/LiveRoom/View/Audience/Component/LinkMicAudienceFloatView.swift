//
//  LinkMicAudienceFloatView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/2.
//

import UIKit
import TUICore
import Combine

class UserImageCell: UICollectionViewCell {
    var user: User? {
        didSet {
            if let url = URL(string: user?.avatarUrl ?? "") {
                avatarImageView.kf.setImage(with: url, placeholder: UIImage.placeholderImage)
            } else {
                avatarImageView.image = .placeholderImage
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

class LinkMicAudienceFloatView: UIView {
  
    private let store: LiveStore
    private let routerStore: RouterStore
    
    private lazy var linkStatus =  store.select(ViewSelectors.getLinkStatus)
    private var cancellableSet = Set<AnyCancellable>()
    
    private var dotsTimer: Timer = Timer()
    private var isViewReady: Bool = false
    init(store: LiveStore, routerStore: RouterStore) {
        self.store = store
        self.routerStore = routerStore
        super.init(frame: .zero)
        updateLabelText()
        subscribeViewState()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private func subscribeViewState() {
        linkStatus
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
        store.select(UserSelectors.getUserList)
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
        cell.user = store.selectCurrent(UserSelectors.getSelfInfo)
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
            let requestId = self.store.selectCurrent(SeatSelectors.getMySeatApplicationId)
            self.store.dispatch(action: SeatActions.cancelApplication(payload: requestId))
            self.routerStore.router(action: .dismiss())
        }
        items.append(item)
        routerStore.router(action: RouterAction.present(.listMenu(items)))
    }
}

private extension String {
    static var toBePassedText: String {
        localized("live.audience.link.float.toBePassed.xxx")
    }
    static var cancelLinkMicRequestText = {
        localized("live.audience.link.confirm.cancelLinkMicRequest")
    }()
}
