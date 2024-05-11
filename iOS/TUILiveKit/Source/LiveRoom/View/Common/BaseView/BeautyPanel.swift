//
//  BeautyPanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/13.
//

import UIKit
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

class BeautyPanel: UIView {
    private var hasRenderView: Bool
    private let engineService: RoomEngineService
    init(engineService: RoomEngineService, hasRenderView: Bool = true) {
        self.engineService = engineService
        self.hasRenderView = hasRenderView
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()
    
    private var isPresent:Bool = false {
        didSet {
            beautyTypeLabel.isHidden = !isPresent
            slider.isHidden = !isPresent
            sliderLabel.isHidden = !isPresent
        }
    }
    
    private var isViewReady: Bool = false
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setView()
        registerObserver()
        backgroundColor = .g2
        isViewReady = true
    }

    private var selfInfo: UserInfo {
        engineService.liveRoomInfo.selfInfo
    }

    private var popupAction: Observable<PopupPanelAction>?

    private var liveRoomInfo:LiveRoomInfo {
        engineService.liveRoomInfo
    }

    private var currentBeautyType: BeautyTypeEvent = .closeClick

    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .titleText
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .center
        view.sizeToFit()
        return view
    }()

    private let previewView: UIView = {
        let view = UIView()
        view.layer.cornerRadius = 16.scale375Width()
        view.layer.masksToBounds = true
        return view
    }()

    private let beautyTypeLabel = {
        let view = UILabel()
        view.textAlignment = .center
        view.textColor = .g6
        view.font = .customFont(ofSize: 12)
        view.adjustsFontSizeToFitWidth = true
        view.isHidden = true
        return view
    }()

    private lazy var slider: UISlider = {
        let view = UISlider()
        view.minimumValue = 0
        view.maximumValue = 9
        view.value = 0
        view.addTarget(self, action: #selector(sliderValueChanged), for: .valueChanged)
        view.setThumbImage(.liveBundleImage("live_slider_icon"), for: .normal)
        view.isHidden = true
        return view
    }()

    private lazy var sliderLabel: UILabel = {
        let view = UILabel()
        view.text = String(Int(self.slider.value))
        view.textAlignment = .center
        view.textColor = .g6
        view.font = .customFont(ofSize: 12)
        view.adjustsFontSizeToFitWidth = true
        view.isHidden = true
        return view
    }()
    
    private lazy var featureCollectionView: FeatureCollectionView = {
        let model = FeatureCollectionViewModel()
        model.items = featureItems
        let config = FeatureCollectionViewDesignConfig()
        config.scrollDirection = .vertical
        config.hasHeader = false
        var collectionView = FeatureCollectionView(model: model, designConfig: config)
        collectionView.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            guard let actionType = action as? BeautyTypeEvent else{ return}
            switch actionType {
            case .closeClick:
                self?.closeBeauty()
            case .buffingClick:
                self?.enableBuffing()
            case .whitenessClick:
                self?.enableWhiteness()
            case .ruddyClick:
                self?.enableRuddy()
            }
        }
        return collectionView
    }()
    
    private var featureItems: [FeatureCollectionViewItem] = {
        var items: [FeatureCollectionViewItem] = []
        let beautyList: [BeautyTypeEvent] = [.closeClick,
                                             .buffingClick,
                                             .whitenessClick,
                                             .ruddyClick,]
        for beauty in beautyList {
            let item = FeatureCollectionViewItem(title: beauty.getString(),
                                                      image: beauty.getImage(),
                                                      isSelected: false,
                                                 action: beauty)
            items.append(item)
        }
        return items
    }()

    func setView() {
        backgroundColor = .b2d
        layer.cornerRadius = 20
        layer.masksToBounds = true

        if hasRenderView {
            engineService.setLocalVideoView(view: previewView)
            engineService.openLocalCamera() {
                
            } onError: { [weak self] code, message in
                let statusString = String(code.rawValue) + "," + message
                self?.makeToast(.localizedReplace(.operateFailedText, replace: statusString))
            }
        }
    }

    func registerObserver() {
        selfInfo.beautyInfo.buffingLevel.addObserver(self) { [weak self] _, _ in
            self?.engineService.setBeautyLevel(Float(self?.selfInfo.beautyInfo.buffingLevel.value ?? 0))
        }

        selfInfo.beautyInfo.whitenessLevel.addObserver(self) { [weak self] _, _ in
            self?.engineService.setWhitenessLevel(Float(self?.selfInfo.beautyInfo.whitenessLevel.value ?? 0))
        }
        selfInfo.beautyInfo.ruddyLevel.addObserver(self) { [weak self] _, _ in
            self?.engineService.setRuddyLevel(Float(self?.selfInfo.beautyInfo.ruddyLevel.value ?? 0))
        }
    }
}

// MARK: Layout

extension BeautyPanel {
    func constructViewHierarchy() {
        addSubview(backButton)
        addSubview(titleLabel)
        if hasRenderView {
            addSubview(previewView)
        }
        addSubview(beautyTypeLabel)
        addSubview(slider)
        addSubview(sliderLabel)
        addSubview(featureCollectionView)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.width.equalToSuperview()
                make.height.equalTo(hasRenderView ? 718.scale375Height() : 374.scale375Width())
                make.centerX.equalToSuperview()
            } else {
                make.width.equalTo(375)
                make.height.equalToSuperview()
                make.trailing.equalToSuperview() 
            }
            make.bottom.equalToSuperview()
        }

        backButton.snp.remakeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }

        titleLabel.snp.remakeConstraints { make in
            make.centerY.equalTo(backButton)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(titleLabel.mm_w)
        }

        if hasRenderView {
            previewView.snp.makeConstraints { make in
                make.top.equalTo(titleLabel.snp.bottom).offset(32.scale375Height())
                make.leading.equalToSuperview().offset(24.scale375Width())
                make.trailing.equalToSuperview().offset(-23.scale375Width())
                make.height.equalTo(328.scale375Height())
            }
        }
        beautyTypeLabel.snp.makeConstraints { make in
            make.trailing.equalTo(slider.snp.leading).offset(-9.scale375())
            if hasRenderView {
                make.top.equalTo(previewView.snp.bottom).offset(16.scale375Height())
            } else {
                make.top.equalTo(titleLabel.snp.bottom).offset(32.scale375Height())
            }
            make.height.equalTo(22.scale375Height())
            make.width.equalTo(24.scale375())
        }

        slider.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(111.scale375())
            make.top.equalTo(beautyTypeLabel.snp.top)
            make.height.equalTo(22.scale375Height())
            make.width.equalTo(189.scale375())
        }

        sliderLabel.snp.makeConstraints { make in
            make.leading.equalTo(slider.snp.trailing).offset(9.scale375())
            make.top.equalTo(beautyTypeLabel.snp.top)
            make.width.equalTo(15.scale375())
            make.height.equalTo(22.scale375Height())
        }

        featureCollectionView.snp.makeConstraints { make in
            make.top.equalTo(sliderLabel.snp.bottom).offset(16.scale375Height())
            make.leading.trailing.equalToSuperview().inset(24.scale375())
            make.height.equalTo(238.scale375())
        }
    }
}

extension BeautyPanel: PopupPanelSubViewProtocol {
    func setAction(_ popupAction: Observable<PopupPanelAction>) {
        self.popupAction = popupAction
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
}

// MARK: Action

extension BeautyPanel {
    @objc func backButtonClick() {
        popupAction?.value = .close
        VideoLinkSettingPanel.shouldUpdateSelf.value = true
    }

    @objc func sliderValueChanged() {
        switch currentBeautyType {
        case .buffingClick:
            selfInfo.beautyInfo.buffingLevel.value = slider.value
        case .whitenessClick:
            selfInfo.beautyInfo.whitenessLevel.value = slider.value
        case .ruddyClick:
            selfInfo.beautyInfo.ruddyLevel.value = slider.value
        default:
            break
        }
        sliderLabel.text = String(Int(slider.value))
    }

    private func closeBeauty() {
        isPresent = false
        currentBeautyType = .closeClick
        selfInfo.beautyInfo.buffingLevel.value = 0
        selfInfo.beautyInfo.whitenessLevel.value = 0
        selfInfo.beautyInfo.ruddyLevel.value = 0
        slider.setValue(0, animated: true)
        sliderLabel.text = String(Int(slider.value))
    }

    private func enableBuffing() {
        isPresent = true
        currentBeautyType = .buffingClick
        beautyTypeLabel.text = .buffingText
        engineService.setBeautyLevel(Float(selfInfo.beautyInfo.buffingLevel.value))
        slider.setValue(Float(selfInfo.beautyInfo.buffingLevel.value), animated: true)
        sliderLabel.text = String(Int(slider.value))
    }

    private func enableWhiteness() {
        isPresent = true
        currentBeautyType = .whitenessClick
        beautyTypeLabel.text = .whitenessText
        engineService.setWhitenessLevel(Float(selfInfo.beautyInfo.whitenessLevel.value))
        slider.setValue(Float(selfInfo.beautyInfo.whitenessLevel.value), animated: true)
        sliderLabel.text = String(Int(slider.value))
    }

    private func enableRuddy() {
        isPresent = true
        currentBeautyType = .ruddyClick
        beautyTypeLabel.text = .ruddyText
        engineService.setRuddyLevel(Float(selfInfo.beautyInfo.ruddyLevel.value))
        slider.setValue(Float(selfInfo.beautyInfo.ruddyLevel.value), animated: true)
        sliderLabel.text = String(Int(slider.value))
    }
}

private extension String {
    static var titleText: String {
        localized("live.beauty.title")
    }

    static var buffingText: String {
        localized("live.beauty.buffing")
    }

    static var closeText: String {
        localized("live.beauty.close")
    }

    static var whitenessText: String {
        localized("live.beauty.whiteness")
    }

    static var ruddyText: String {
        localized("live.beauty.ruddy")
    }
    
    static var operateFailedText: String {
        localized("live.operation.fail.xxx")
    }
}
