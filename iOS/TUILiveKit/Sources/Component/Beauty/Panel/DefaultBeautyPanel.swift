//
//  DefaultBeautyPanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/13.
//

import UIKit
import Combine
import RTCCommon
import AtomicXCore
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

enum BeautyTypeEvent {
    case closeClick
    case buffingClick
    case whitenessClick
    case ruddyClick
    
    func getString() -> String {
        switch self {
        case .closeClick:
            return internalLocalized("turn off")
        case .buffingClick:
            return internalLocalized("Microdermabrasion")
        case .whitenessClick:
            return internalLocalized("Whitening")
        case .ruddyClick:
            return internalLocalized("Rosy")
        }
    }

    func getImage() -> UIImage? {
        switch self {
        case .closeClick:
            return internalImage("live_beauty_close")
        case .buffingClick:
            return internalImage("live_beauty_buffing")
        case .whitenessClick:
            return internalImage("live_beauty_whiteness")
        case .ruddyClick:
            return internalImage("live_beauty_ruddy")
        }
    }
}

class DefaultBeautyPanel: UIView {
    var backClosure: (() ->Void)?
    private var cancellableSet = Set<AnyCancellable>()
    private var hasRenderView: Bool
    init(hasRenderView: Bool = false) {
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
    
    private var currentSmoothLevel: Float {
        baseBeautyStore.state.value.smoothLevel
    }
    
    private var currentWhitenessLevel: Float {
        baseBeautyStore.state.value.whitenessLevel
    }
    
    private var currentRuddyLevel: Float {
        baseBeautyStore.state.value.ruddyLevel
    }
    
    private var isCloseBeauty: Bool {
        return currentSmoothLevel == 0
            && currentWhitenessLevel == 0
            && currentRuddyLevel == 0
    }
    
    private var isViewReady: Bool = false
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        updatePreview()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setView()
        bindInteraction()
        isViewReady = true
    }
    
    private var currentBeautyType: BeautyTypeEvent = .closeClick
    
    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(internalImage("live_back_icon"), for: .normal)
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
        view.setThumbImage(internalImage("live_slider_icon"), for: .normal)
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
    
    private let config: FeatureCollectionViewDesignConfig = {
        let config = FeatureCollectionViewDesignConfig()
        config.scrollDirection = .vertical
        config.hasHeader = false
        return config
    }()
    
    private lazy var featureCollectionView: FeatureCollectionView = {
        let model = FeatureCollectionViewModel()
        model.items = featureItems
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
                item.isSelected = isCloseBeauty
            } else if beauty == .buffingClick {
                item.isSelected = !isCloseBeauty
            }
            items.append(item)
        }
        return items
    }()
    
    func setView() {
        backgroundColor = .g2
        layer.cornerRadius = 20
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        
        if !isCloseBeauty {
            enableBuffing()
        }
    }
    
    private func updatePreview() {
        if hasRenderView {
            TUIRoomEngine.sharedInstance().setLocalVideoView(view: previewView)
            deviceStore.openLocalCamera(isFront: true, completion: nil)
        }
    }
}

// MARK: Layout
extension DefaultBeautyPanel {
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
        
        let height = config.itemSize.height * CGFloat(featureItems.count / 5 + 1) + CGFloat(featureItems.count - 1) * config.itemSpacing
        featureCollectionView.snp.makeConstraints { make in
            make.top.equalTo(sliderLabel.snp.bottom).offset(16.scale375Height())
            make.leading.trailing.equalToSuperview().inset(24.scale375())
            make.height.equalTo(min(height, 238).scale375Height())
            make.bottom.equalToSuperview().offset(-20.scale375Height())
        }
    }
}

// MARK: Action
extension DefaultBeautyPanel {
    private func  bindInteraction() {
        baseBeautyStore.setSmoothLevel(smoothLevel: currentSmoothLevel)
        baseBeautyStore.setWhitenessLevel(whitenessLevel: currentWhitenessLevel)
        baseBeautyStore.setRuddyLevel(ruddyLevel: currentRuddyLevel)
    }
    
    @objc func backButtonClick(sender: UIButton) {
        backClosure?()
    }
    
    @objc func sliderValueChanged() {
        switch currentBeautyType {
        case .buffingClick:
            baseBeautyStore.setSmoothLevel(smoothLevel: slider.value)
        case .whitenessClick:
            baseBeautyStore.setWhitenessLevel(whitenessLevel: slider.value)
        case .ruddyClick:
            baseBeautyStore.setRuddyLevel(ruddyLevel: slider.value)
        default:
            break
        }
        sliderLabel.text = String(Int(slider.value))
    }
    
    private func closeBeauty() {
        isPresent = false
        currentBeautyType = .closeClick
        resetBeauty()
        slider.setValue(0, animated: true)
        sliderLabel.text = String(Int(slider.value))
    }
    
    private func enableBuffing() {
        isPresent = true
        currentBeautyType = .buffingClick
        beautyTypeLabel.text = .buffingText
        let smoothLevel = baseBeautyStore.state.value.smoothLevel
        baseBeautyStore.setSmoothLevel(smoothLevel: smoothLevel)
        slider.setValue(Float(smoothLevel), animated: true)
        sliderLabel.text = String(Int(slider.value))
    }
    
    private func enableWhiteness() {
        isPresent = true
        currentBeautyType = .whitenessClick
        beautyTypeLabel.text = .whitenessText
        let whitenessLevel = baseBeautyStore.state.value.whitenessLevel
        baseBeautyStore.setWhitenessLevel(whitenessLevel: whitenessLevel)
        slider.setValue(Float(whitenessLevel), animated: true)
        sliderLabel.text = String(Int(slider.value))
    }
    
    private func enableRuddy() {
        isPresent = true
        currentBeautyType = .ruddyClick
        beautyTypeLabel.text = .ruddyText
        let ruddyLevel = baseBeautyStore.state.value.ruddyLevel
        baseBeautyStore.setRuddyLevel(ruddyLevel: ruddyLevel)
        slider.setValue(Float(ruddyLevel), animated: true)
        sliderLabel.text = String(Int(slider.value))
    }
    
    private func resetBeauty() {
        // TODO: reset api
        baseBeautyStore.setRuddyLevel(ruddyLevel: 0)
        baseBeautyStore.setSmoothLevel(smoothLevel: 0)
        baseBeautyStore.setWhitenessLevel(whitenessLevel: 0)
    }
}

extension DefaultBeautyPanel {
    var deviceStore: DeviceStore {
        return DeviceStore.shared
    }
    
    var baseBeautyStore: BaseBeautyStore {
        return BaseBeautyStore.shared
    }
}

private extension String {
    static let titleText = internalLocalized("One-click beauty")
    static let buffingText = internalLocalized("Microdermabrasion")
    static let whitenessText = internalLocalized("Whitening")
    static let ruddyText = internalLocalized("Rosy")
    }
