//
//  LinkMicAnchorFloatView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/4.
//

import SnapKit
import UIKit
import Combine
import RTCCommon
import RTCRoomEngine
import LiveStreamCore

class LinkMicAnchorFloatView: UIView {
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    private var cancellableSet = Set<AnyCancellable>()
    

    private var applyList: [TUIUserInfo] = []
    lazy var tipsLabel: UILabel = {
        let label = UILabel()
        label.textColor = .flowKitWhite
        label.font = .customFont(ofSize: 14)
        label.textAlignment = .center
        label.adjustsFontSizeToFitWidth = true
        return label
    }()

    lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .horizontal
        layout.itemSize = CGSize(width: 40.scale375(), height: 40.scale375())
        layout.minimumLineSpacing = -16.scale375()
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
    
    
    init(manager: LiveStreamManager, routerManager: LSRouterManager) {
        self.manager = manager
        self.routerManager = routerManager
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        subscribeSeatState()
        isViewReady = true
    }

    private func subscribeSeatState() {
        manager.subscribeCoreViewState(StateSelector(keyPath: \CoGuestState.applicantList))
            .receive(on: RunLoop.main)
            .sink { [weak self] applyList in
                guard let self = self else { return }
                self.applyList = Array(applyList)
                self.tipsLabel.text = .localizedReplace(.applyLinkMicCount, replace: String(applyList.count))
                self.updateView()
            }
            .store(in: &cancellableSet)
    }
    
    private func updateView() {
        collectionView.reloadData()
        collectionView.snp.updateConstraints { make in
            switch applyList.count {
            case 1:
                make.width.equalTo(40.scale375())
            case 2:
                make.width.equalTo(56.scale375())
            default:
                make.width.equalTo(76.scale375())
            }
        }
    }
}

// MARK: Layout

extension LinkMicAnchorFloatView {
    func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 10
        layer.masksToBounds = true
        layer.borderWidth = 1
        layer.borderColor = UIColor.flowKitWhite.withAlphaComponent(0.2).cgColor
        
        addSubview(tipsLabel)
        addSubview(collectionView)
    }

    func activateConstraints() {
        collectionView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(18.scale375Width())
            make.centerX.equalToSuperview()
            make.height.equalTo(40.scale375())
            make.width.equalTo(56.scale375())
        }

        tipsLabel.snp.makeConstraints { make in
            make.top.equalTo(collectionView.snp.bottom).offset(6.scale375Width())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(20.scale375())
        }
    }
}

// MARK: - UICollectionViewDataSource

extension LinkMicAnchorFloatView: UICollectionViewDataSource {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }

    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return min(applyList.count, 3)
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: UserImageCell.cellReuseIdentifier, for: indexPath) as! UserImageCell
        
        if indexPath.row < 2 {
            cell.user = applyList[indexPath.row]
        } else {
            let user = TUIUserInfo()
            cell.user = user
            cell.setImage(image: internalImage("live_more_audience_icon"))
        }
        return cell
    }
}

// MARK: Action

extension LinkMicAnchorFloatView {
    @objc func tapAction() {
        routerManager.router(action: .present(.liveLinkControl))
    }
}

private extension String {
    static var applyLinkMicCount: String {
        internalLocalized("Link Application(xxx)")
    }
}
