//
//  AudienceSettingPanel.swift
//  TUILiveKit
//
//  Created by jack on 2025/8/29.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine
import AtomicXCore

class AudienceSettingPanel: UIView {
    
    enum SettingItemType {
        case resolution
        case dashboard
        
        var title: String {
            switch self {
            case .resolution:
                return .resolutionText
            case .dashboard:
                return .dashboardText
            }
        }
        
        var icon: UIImage? {
            switch self {
            case .resolution:
                return internalImage("live_video_resolution_icon")
            case .dashboard:
                return internalImage("live_setting_stream_dashboard")
            }
        }
    }
    
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
    private var items:[SettingItemType] = [.dashboard]

    private var cancellableSet = Set<AnyCancellable>()
    
    public init(manager: AudienceManager, routerManager: AudienceRouterManager) {
        self.manager = manager
        self.routerManager = routerManager
        super.init(frame: .zero)
        backgroundColor = .bgOperateColor
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .settingTitleText
        view.textColor = .textPrimaryColor
        view.font = .customFont(ofSize: 16, weight: .medium)
        view.textAlignment = .center
        return view
    }()
    
    private lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.scrollDirection = .vertical
        let view = UICollectionView(frame: .zero, collectionViewLayout: layout)
        view.backgroundColor = .clear
        view.dataSource = self
        view.delegate = self
        view.showsVerticalScrollIndicator = false
        view.showsHorizontalScrollIndicator = false
        view.contentInset = UIEdgeInsets(top: 0, left: 24, bottom: 0, right: 24)
        view.register(AudienceSettingPanelCell.self, forCellWithReuseIdentifier: AudienceSettingPanelCell.CellId)
        return view
    }()
    
    private weak var popupViewController: PopupViewController?
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        subscribeCoGuestState()
        isViewReady = true
    }
    
    private func subscribeCoGuestState() {
        manager.subscribeState(StateSelector(keyPath: \AudienceCoGuestState.coGuestStatus))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] coGuestStatus in
                guard let self = self else {
                    return
                }
                self.reloadItems()
            }
            .store(in: &cancellableSet)
    }
    
    private func reloadItems() {
        let enableMultiQuality = manager.mediaState.playbackQualityList.count > 1 && manager.coGuestState.coGuestStatus == .none
        let containsResolution = items.contains { $0 == .resolution }
        if enableMultiQuality {
            if !containsResolution {
                items.append(.resolution)
            }
        } else {
            items.removeAll(where: { $0 == .resolution })
        }
        collectionView.reloadData()
    }
}


// MARK: Layout
private extension AudienceSettingPanel {
    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(collectionView)
    }
    
    func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20)
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24)
        }
        collectionView.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
            make.bottom.equalToSuperview().inset(20)
            make.height.equalTo(80)
            make.leading.trailing.equalToSuperview().inset(16)
        }
    }
}

// MARK: - Action
extension AudienceSettingPanel {
    
    private func selectResolution() {
        routerManager.router(action: .dismiss(.panel, completion: { [weak self] in
            guard let self = self else { return }
            self.routerManager.router(
                action: .present(.videoQualitySelection(
                    resolutions: manager.mediaState.playbackQualityList,
                    selectedClosure: { [weak self] (quality) in
                        guard let self = self else { return }
                        self.manager.mediaManager.switchPlaybackQuality(quality: quality)
                        self.routerManager.router(action: .dismiss())
                    })))
        }))
    }
    
    private func selectDashBoard() {
        routerManager.router(action: .dismiss(.panel, completion: { [weak self] in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.streamDashboard))
        }))
    }
}

// MARK: - UICollectionViewDataSource
extension AudienceSettingPanel: UICollectionViewDataSource {
    
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return items.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: AudienceSettingPanelCell.CellId, for: indexPath) as! AudienceSettingPanelCell
        let item = items[indexPath.item]
        cell.titleLabel.text = item.title
        cell.imageView.image = item.icon
        return cell
    }
    
}

// MARK: - UICollectionViewDelegateFlowLayout
extension AudienceSettingPanel: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        return CGSize(width: 56, height: 80)
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, minimumLineSpacingForSectionAt section: Int) -> CGFloat {
        return 12
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, minimumInteritemSpacingForSectionAt section: Int) -> CGFloat {
        return 12
    }
    
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        let item = items[indexPath.item]
        if item == .resolution {
            selectResolution()
        }
        if item == .dashboard {
            selectDashBoard()
        }
    }
}

class AudienceSettingPanelCell: UICollectionViewCell {
    
    static let CellId: String = "AudienceSettingPanelCell"
    
    let titleLabel: UILabel = {
        let label = UILabel()
        label.textAlignment = .center
        label.adjustsFontSizeToFitWidth = true
        label.textColor = .white
        label.font = .systemFont(ofSize: 12)
        return label
    }()
    
    let imageView: UIImageView = {
        let view = UIImageView()
        return view
    }()
    
    let imageBgView: UIView = {
        let view = UIView()
        view.layer.cornerRadius = 8
        view.layer.masksToBounds = true
        view.backgroundColor = .bgEntrycardColor
        return view
    }()
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(imageBgView)
        imageBgView.addSubview(imageView)
    }
    
    private func activateConstraints() {
        imageBgView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.width.height.equalTo(56)
            make.top.equalToSuperview()
        }
        imageView.snp.makeConstraints { make in
            make.center.equalToSuperview()
        }
        titleLabel.snp.makeConstraints { make in
            make.bottom.equalToSuperview()
            make.centerX.equalToSuperview()
            make.leading.trailing.equalToSuperview()
        }
        
    }
}


private extension String {
    static let settingTitleText: String = internalLocalized("More Features")
    
    static let resolutionText: String = internalLocalized("Resolution")
    static let dashboardText: String = internalLocalized("Dashboard")
}
