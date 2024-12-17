//
//  VideoSettingPanel.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/25.
//

import Foundation
import RTCRoomEngine
import Combine
import RTCCommon
import LiveStreamCore
import TUICore

class VideoSettingPanel: UIView {
    
    private let mediaManager: MediaManager
    private let routerManager: LSRouterManager
    private var cancellableSet: Set<AnyCancellable> = []
    
    private var videoEncParams: TUIRoomVideoEncoderParams = TUIRoomVideoEncoderParams()
    
    init(routerManager: LSRouterManager, mediaManager: MediaManager) {
        self.mediaManager = mediaManager
        self.routerManager = routerManager
        self.videoEncParams = mediaManager.mediaState.videoEncParams.currentEnc
        super.init(frame: .zero)
        self.initDataSource()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var basicDataSource: [VideoSettingItem] = []
    private var moreDataSource: [VideoSettingItem] = []
    
    private let titleLabel: UILabel = {
        let label = UILabel()
        label.text = .settingTitleText
        label.font = .customFont(ofSize: 16.0, weight: .medium)
        label.textColor = .g7
        label.textAlignment = .center
        return label
    }()
    
    private lazy var collectionView: UICollectionView = {
        let layout = VideoSettingLayout()
        layout.minimumLineSpacing = 0
        layout.minimumInteritemSpacing = 0
        layout.sectionInset = UIEdgeInsets(top: 0, left: 20, bottom: 0, right: 20)
        layout.scrollDirection = .vertical
        let view = UICollectionView(frame: .zero, collectionViewLayout: layout)
        view.backgroundColor = .clear
        view.delegate = self
        view.dataSource = self
        view.register(VideoSettingButtonCell.self,
                      forCellWithReuseIdentifier: VideoSettingButtonCell.CellID)
        view.register(VideoSettingSliderCell.self,
                      forCellWithReuseIdentifier: VideoSettingSliderCell.CellID)
        view.register(VideoSettingSwitchCell.self,
                      forCellWithReuseIdentifier: VideoSettingSwitchCell.CellID)
        view.register(ViewSettingTitleHeaderView.self,
                      forSupplementaryViewOfKind: UICollectionView.elementKindSectionHeader,
                      withReuseIdentifier: ViewSettingTitleHeaderView.ReusableID)
        return view
    }()
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupViewStyle()
    }
}

// MARK: - UI
extension VideoSettingPanel {
    
    private func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(collectionView)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(10)
            make.centerX.equalToSuperview()
        }
        collectionView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.top.equalTo(titleLabel.snp.bottom).offset(10)
            make.height.equalTo(400)
            make.bottom.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        guard let _ = TUICore.getService(.TUICore_VideoAdvanceService) else { return }
        titleLabel.isUserInteractionEnabled = true
        let longTap = UILongPressGestureRecognizer(target: self, action: #selector(enableAdvancedVideo))
        titleLabel.addGestureRecognizer(longTap)
    }
    
    private func setupViewStyle() {
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
}

// MARK: - Private
extension VideoSettingPanel {

    private func initDataSource() {
        initBasicSource()
        initMoreDataSource()
    }
    
    private func initBasicSource() {
        let resolutionItem = VideoSettingButtonItem(title: .resolutionText, value: videoEncParams.videoResolution.getString()) { [weak self] in
            guard let self = self else { return }
            self.selectVideoResolution()
        }
        basicDataSource.append(resolutionItem)
        let bitrateScope = getBitrateScope(from: videoEncParams.videoResolution)
        var bitrateItem = VideoSettingSliderItem(title: .videoBitrateText,
                                                 value: videoEncParams.bitrate,
                                                 minValue: bitrateScope.minValue,
                                                 maxValue: bitrateScope.maxValue,
                                                 sliderUnit: 100,
                                                 sliderUnitText: "Kbps")
        bitrateItem.valueDidChanged = { [weak self] bitrate in
            guard let self = self else { return }
            self.videoEncParams.bitrate = bitrate
            self.updateVideoEncParams()
        }
        basicDataSource.append(bitrateItem)
        var fpsItem = VideoSettingSliderItem(title: .fpsText,
                                             value: videoEncParams.fps,
                                             minValue: 15,
                                             maxValue: 60,
                                             sliderUnit: 1,
                                             sliderUnitText: "FPS")
        fpsItem.valueDidChanged = { [weak self] fps in
            guard let self = self else { return }
            self.videoEncParams.fps = fps
            self.updateVideoEncParams()
        }
        basicDataSource.append(fpsItem)
    }
    
    private func reloadBasicSettings() {
        UIView.performWithoutAnimation { [weak self] in
            guard let self = self else { return }
            self.collectionView.reloadSections(IndexSet(integer: 0))
        }
    }
    
    private func initMoreDataSource() {
        let mirrorItem = VideoSettingSwitchItem(title: .mirrorText,
                                                isOn: mediaManager.mediaState.isMirrorEnabled,
                                                action: { [weak self] isOn in
            guard let self = self else { return }
            self.mediaManager.enableMirror(isOn)
        })
        moreDataSource.append(mirrorItem)
        if mediaManager.mediaState.videoAdvanceSettings.isVisible {
            addUltimateItem()
            addH265Item()
        }
    }

    private func reloadMoreSettings() {
        let ultimateIndex = addUltimateItem()
        let h265Index = addH265Item()
        collectionView.performBatchUpdates { [weak self] in
            guard let self = self else { return }
            let items = [
                ultimateIndex,
                h265Index,
            ]
            self.collectionView.insertItems(at: items)
        }
    }
    
    @discardableResult
    private func addUltimateItem() -> IndexPath {
        let ultimateItem = VideoSettingSwitchItem(title: .ultimateText,
                                                  isOn: mediaManager.mediaState.videoAdvanceSettings.isUltimateEnabled,
                                                  action: { [weak self] enable in
            guard let self = self else { return }
            self.mediaManager.enableUltimate(enable)
        })
        moreDataSource.append(ultimateItem)
        return IndexPath(item: moreDataSource.count-1, section: 1)
    }
    
    @discardableResult
    private func addH265Item() -> IndexPath {
        let h265Item = VideoSettingSwitchItem(title: "H265",
                                              isOn: mediaManager.mediaState.videoAdvanceSettings.isH265Enabled,
                                              action: { [weak self] enable in
            guard let self = self else { return }
            self.mediaManager.enableH265(enable)
        })
        moreDataSource.append(h265Item)
        return IndexPath(item: moreDataSource.count-1, section: 1)
    }
    
    private func selectVideoResolution() {
        var items: [ActionItem] = []
        let designConfig = ActionItemDesignConfig()
        var config: ActionItemDesignConfig
        let categoryList: [TUIVideoQuality] = [.quality360P, .quality540P, .quality720P, .quality1080P]
        for category in categoryList {
            if category == categoryList.last {
                config = ActionItemDesignConfig(lineWidth: 7)
            } else {
                config = designConfig
            }
            let item = ActionItem(title: category.getString(), designConfig: config, actionClosure: { [weak self] _ in
                guard let self = self else { return }
                self.updateVideoQuality(category)
                self.routerManager.router(action: .dismiss())
            })
            items.append(item)
        }
        routerManager.router(action: .present(.listMenu(ActionPanelData(items: items))))
    }
    
    private func updateVideoQuality(_ resolution: TUIVideoQuality) {
        videoEncParams.videoResolution = resolution
        if var resolutionItem = basicDataSource[0] as? VideoSettingButtonItem {
            resolutionItem.value = resolution.getString()
            basicDataSource[0] = resolutionItem
        }
        if var bitrateItem = basicDataSource[1] as? VideoSettingSliderItem {
            let bitrateScope = getBitrateScope(from: resolution)
            bitrateItem.minValue = bitrateScope.minValue
            bitrateItem.maxValue = bitrateScope.maxValue
            if bitrateItem.value > bitrateScope.maxValue {
                bitrateItem.value = bitrateScope.maxValue
            }
            if bitrateItem.value < bitrateScope.minValue {
                bitrateItem.value = bitrateScope.minValue
            }
            videoEncParams.bitrate = bitrateItem.value
            basicDataSource[1] = bitrateItem
        }
        updateVideoEncParams()
        reloadBasicSettings()
    }
    
    private func getBitrateScope(from resolution: TUIVideoQuality) -> (minValue: Int, maxValue:Int) {
        switch resolution {
        case .quality1080P:
            return (1000, 10000)
        case .quality720P:
            return (500, 6000)
        case .quality540P:
            return (100, 2000)
        case .quality360P:
            return (100, 1500)
        default:
            return (1000, 10000)
        }
    }
}

// MARK: - Update Video Encoder Params
extension VideoSettingPanel {
    
    private func updateVideoEncParams() {
        mediaManager.updateVideoEncParams(videoEncParams)
    }
}

// MARK: - Actions
extension VideoSettingPanel {
    
    @objc
    private func enableAdvancedVideo() {
        guard !mediaManager.mediaState.videoAdvanceSettings.isVisible else {
            return
        }
        mediaManager.enableAdvancedVisible(true)
        reloadMoreSettings()
    }
}

// MARK: - UICollectionViewDataSource
extension VideoSettingPanel: UICollectionViewDataSource {
    
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 2
    }
    
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        if section == 0 {
            return basicDataSource.count
        }
        return moreDataSource.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        var itemData: VideoSettingItem
        if indexPath.section == 0 {
            itemData = basicDataSource[indexPath.item]
        } else {
            itemData = moreDataSource[indexPath.item]
        }
        if itemData.cellType == .slider {
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: VideoSettingSliderCell.CellID, for: indexPath) as! VideoSettingSliderCell
            cell.updateUI(item: itemData)
            return cell
        }
        if itemData.cellType == .switcher {
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: VideoSettingSwitchCell.CellID, for: indexPath) as! VideoSettingSwitchCell
            cell.updateUI(item: itemData)
            return cell
        }
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: VideoSettingButtonCell.CellID, for: indexPath) as! VideoSettingButtonCell
        cell.updateUI(item: itemData)
        return cell
    }
}

// MARK: - UICollectionViewDelegateFlowLayout
extension VideoSettingPanel: UICollectionViewDelegateFlowLayout {
    
    func collectionView(_ collectionView: UICollectionView, viewForSupplementaryElementOfKind kind: String, at indexPath: IndexPath) -> UICollectionReusableView {
        let view = collectionView.dequeueReusableSupplementaryView(ofKind: UICollectionView.elementKindSectionHeader, withReuseIdentifier: ViewSettingTitleHeaderView.ReusableID, for: indexPath) as! ViewSettingTitleHeaderView
        if indexPath.section == 0 {
            view.titleLabel.text = .sharpnessText
        } else {
            view.titleLabel.text = .moreSettingText
        }
        return view
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, referenceSizeForHeaderInSection section: Int) -> CGSize {
        return CGSize(width: Screen_Width, height: 40)
    }
    
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        if indexPath.section == 0 {
            if indexPath.item == 1 || indexPath.item == 2 {
                return CGSize(width: Screen_Width - 20*2, height: 65)
            }
        }
        return CGSize(width: Screen_Width - 20*2, height: 50)
    }
}


fileprivate extension String {
    static var settingTitleText: String {
        localized("live.anchor.setting.video.parameters")
    }
    
    static var sharpnessText: String {
        localized("live.anchor.setting.video.sharpness")
    }
    
    static let ultimateText = localized("live.anchor.setting.video.ultimate")
    static let resolutionText = localized("live.anchor.setting.video.resolution")
    static let fpsText = localized("live.anchor.setting.video.fps")
    static let videoBitrateText = localized("live.anchor.setting.video.bitrate")
    static let mirrorText = localized("live.anchor.setting.mirror")
    static let moreSettingText = localized("live.anchor.setting.more.setting")
    
    // MARK: - Video Advance API Extension
    static let TUICore_VideoAdvanceService = "TUICore_VideoAdvanceService"
}
