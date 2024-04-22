//
//  LinkMicAnchorFloatView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/4.
//

import SnapKit
import UIKit
import Combine

class LinkMicAnchorFloatView: UIView {
    private var cancellable = Set<AnyCancellable>()
    private var engineService: RoomEngineService
    private var liveRoomInfo:LiveRoomInfo {
        engineService.liveRoomInfo
    }
    private var isViewReady: Bool = false
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        updateView()
        engineService.liveKitStore.$applyLinkAudienceList
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.updateView()
                let audienceCount = self.engineService.liveKitStore.applyLinkAudienceList.count
                self.tipsLabel.text = .localizedReplace(.applyLinkMicCount, replace: String(audienceCount))
            }.store(in: &cancellable)
    }

    lazy var tipsLabel: UILabel = {
        let label = UILabel()
        label.textColor = .flowKitWhite
        label.text = .localizedReplace(.applyLinkMicCount, replace: String(self.engineService.liveKitStore.applyLinkAudienceList.count))
        label.font = .customFont(ofSize: 14)
        label.textAlignment = .center
        label.adjustsFontSizeToFitWidth = true
        return label
    }()

    lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .horizontal
        layout.itemSize = CGSize(width: 36.scale375(), height: 36.scale375())
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

    private func updateView() {
        collectionView.reloadData()
        let audienceCount = engineService.liveKitStore.applyLinkAudienceList.count
        collectionView.snp.updateConstraints { make in
            switch audienceCount {
            case 1:
                make.width.equalTo(36.scale375())
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
            make.height.equalTo(36.scale375())
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
        return min(engineService.liveKitStore.applyLinkAudienceList.count, 3)
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: UserImageCell.cellReuseIdentifier, for: indexPath) as! UserImageCell
        
        if indexPath.row < 2 {
            cell.userInfo = engineService.liveKitStore.applyLinkAudienceList[indexPath.row]
        } else {
            let userInfo = UserInfo()
            cell.userInfo = userInfo
            cell.setImage(image: .liveBundleImage("live_more_audience_icon"))
        }
        return cell
    }
}

// MARK: Action

extension LinkMicAnchorFloatView {
    @objc func tapAction() {
        PopupPanelController.alertView(AnchorLinkControlPanel(engineService: engineService))
    }
}

private extension String {
    static var applyLinkMicCount: String {
        localized("live.audience.link.float.applyLinkMicCount.xxx")
    }
}
