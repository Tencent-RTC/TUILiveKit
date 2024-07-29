//
//  BeautyPanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/13.
//

import UIKit
import Combine
import RTCCommon
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

class BeautyPanel: UIView {
    var backClosure: (() ->Void)?
    private var store: LiveStore
    private var routerStore: RouterStore
    private var cancellableSet = Set<AnyCancellable>()
    private var hasRenderView: Bool
    init(hasRenderView: Bool = true, store: LiveStore, routerStore: RouterStore) {
        self.hasRenderView = hasRenderView
        self.store = store
        self.routerStore = routerStore
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
    
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setView()
        bindInteraction()
        isViewReady = true
    }
    
    private var selfInfo: User {
        store.selectCurrent(UserSelectors.getSelfInfo)
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
    
    private lazy var featureItems: [FeatureCollectionViewItem] = {
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
            if beauty == .closeClick {
                item.isSelected = store.beautyState.isCloseBeauty()
            } else if beauty == .buffingClick {
                item.isSelected = !store.beautyState.isCloseBeauty()
            }
            items.append(item)
        }
        return items
    }()
    
    func setView() {
        backgroundColor = .g2
        layer.cornerRadius = 20
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        
        if hasRenderView {
            store.dispatch(action: MediaActions.updateLocalVideoView(payload: previewView))
            store.dispatch(action: MediaActions.operateCamera(payload: true))
        }
        if !store.beautyState.isCloseBeauty() {
            enableBuffing()
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
                make.height.equalTo(hasRenderView ? 718.scale375Height() : 374.scale375Height())
            } else {
                make.width.equalTo(375.scale375())
            }
            make.edges.equalToSuperview()
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
        }
        
        slider.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalTo(beautyTypeLabel.snp.top)
            make.height.equalTo(22.scale375Height())
            make.width.equalTo(189.scale375())
        }
        
        sliderLabel.snp.makeConstraints { make in
            make.leading.equalTo(slider.snp.trailing).offset(9.scale375())
            make.top.equalTo(beautyTypeLabel.snp.top)
            make.height.equalTo(22.scale375Height())
        }
        
        featureCollectionView.snp.makeConstraints { make in
            make.top.equalTo(sliderLabel.snp.bottom).offset(16.scale375Height())
            make.leading.trailing.equalToSuperview().inset(24.scale375())
            make.height.equalTo(238.scale375())
        }
    }
}

// MARK: Action
extension BeautyPanel {
    private func  bindInteraction() {
            let smoothLevel = store.selectCurrent(BeautySelectors.getSmoothLevel)
            store.dispatch(action: BeautyActions.setSmoothLevel(payload: smoothLevel))
            let whitenessLevel = store.selectCurrent(BeautySelectors.getWhitenessLevel)
            store.dispatch(action: BeautyActions.setWhitenessLevel(payload: whitenessLevel))
            let ruddyLevel = store.selectCurrent(BeautySelectors.getRuddyLevel)
            store.dispatch(action: BeautyActions.setRuddyLevel(payload: ruddyLevel))
        }
    
    @objc func backButtonClick(sender: UIButton) {
        backClosure?()
    }
    
    @objc func sliderValueChanged() {
        switch currentBeautyType {
            case .buffingClick:
                store.dispatch(action: BeautyActions.setSmoothLevel(payload: Int(slider.value)))
            case .whitenessClick:
                store.dispatch(action: BeautyActions.setWhitenessLevel(payload: Int(slider.value)))
            case .ruddyClick:
                store.dispatch(action: BeautyActions.setRuddyLevel(payload: Int(slider.value)))
            default:
                break
        }
        sliderLabel.text = String(Int(slider.value))
    }
    
    private func closeBeauty() {
        isPresent = false
        currentBeautyType = .closeClick
        store.dispatch(action: BeautyActions.setWhitenessLevel(payload: 0))
        store.dispatch(action: BeautyActions.setSmoothLevel(payload: 0))
        store.dispatch(action: BeautyActions.setRuddyLevel(payload: 0))
        slider.setValue(0, animated: true)
        sliderLabel.text = String(Int(slider.value))
    }
    
    private func enableBuffing() {
        isPresent = true
        currentBeautyType = .buffingClick
        beautyTypeLabel.text = .buffingText
        let smoothLevel = store.beautyState.smoothLevel
        store.dispatch(action: BeautyActions.setSmoothLevel(payload: smoothLevel))
        slider.setValue(Float(smoothLevel), animated: true)
        sliderLabel.text = String(Int(slider.value))
    }
    
    private func enableWhiteness() {
        isPresent = true
        currentBeautyType = .whitenessClick
        beautyTypeLabel.text = .whitenessText
        let whitenessLevel = store.beautyState.whitenessLevel
        store.dispatch(action: BeautyActions.setWhitenessLevel(payload: whitenessLevel))
        slider.setValue(Float(whitenessLevel), animated: true)
        sliderLabel.text = String(Int(slider.value))
    }
    
    private func enableRuddy() {
        isPresent = true
        currentBeautyType = .ruddyClick
        beautyTypeLabel.text = .ruddyText
        let ruddyLevel = store.beautyState.ruddyLevel
        store.dispatch(action: BeautyActions.setRuddyLevel(payload: ruddyLevel))
        slider.setValue(Float(ruddyLevel), animated: true)
        sliderLabel.text = String(Int(slider.value))
    }
}

private extension BeautyState {
    func isCloseBeauty() -> Bool {
        return (smoothLevel == 0) && (whitenessLevel == 0) && (ruddyLevel == 0)
    }
}

private extension String {
    static let titleText = localized("live.beauty.title")
    static let buffingText = localized("live.beauty.buffing")
    static let closeText = localized("live.beauty.close")
    static let whitenessText = localized("live.beauty.whiteness")
    static let ruddyText = localized("live.beauty.ruddy")
    static let operateFailedText = localized("live.operation.fail.xxx")
}
