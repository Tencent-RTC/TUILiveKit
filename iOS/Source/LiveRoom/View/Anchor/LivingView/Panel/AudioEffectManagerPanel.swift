//
//  AudioEffectManagerPanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/11/20.
//

import UIKit

class AudioEffectManagerPanel: UIView {
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady = false

    private var popupAction: Observable<PopupPanelAction>?

    private var backgroundMusicItemView: AudioSettingListItem?
    private var musicPitchItemView: AudioSettingListItem?

    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        registerObserver()
        isViewReady = true
    }
    
    private var liveRoomInfo:LiveRoomInfo {
        engineService.liveRoomInfo
    }
    private let engineService: RoomEngineService
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private let scrollView: UIScrollView = {
        let scrollView = UIScrollView()
        scrollView.backgroundColor = .clear
        scrollView.showsVerticalScrollIndicator = false
        scrollView.showsHorizontalScrollIndicator = false
        return scrollView
    }()

    private let contentView: UIView = {
        let view = UIView()
        return view
    }()

    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .audioEffectTitleText
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .center
        view.sizeToFit()
        return view
    }()

    private let advancedFeatureStackView: UIStackView = {
        let view = UIStackView()
        view.backgroundColor = .g3.withAlphaComponent(0.3)
        view.layer.cornerRadius = 20
        view.layer.masksToBounds = true
        view.axis = .vertical
        view.alignment = .center
        view.distribution = .equalSpacing
        view.spacing = 0
        return view
    }()

    private let audioSettingLabel: UILabel = {
        let view = UILabel()
        view.text = .audioSettingText
        view.textColor = .g7
        view.font = .customFont(ofSize: 14)
        view.textAlignment = .left
        return view
    }()

    private let audioSettingStackView: UIStackView = {
        let view = UIStackView()
        view.backgroundColor = .g3.withAlphaComponent(0.3)
        view.layer.cornerRadius = 20
        view.layer.masksToBounds = true
        view.axis = .vertical
        view.alignment = .center
        view.distribution = .equalSpacing
        view.spacing = 0
        return view
    }()

    private lazy var changerCollectionView: FeatureCollectionView = {
        let model = FeatureCollectionViewModel()
        model.items = changerItems
        var collectionView = FeatureCollectionView(headerTitle: .changerText, model: model)
        collectionView.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            if let changerType = action as? ChangerTypeEvent {
                self?.engineService.setVoiceChangerType(changerType.getChangerType())
            }
        }
        return collectionView
    }()

    private lazy var reverbCollectionView: FeatureCollectionView = {
        let model = FeatureCollectionViewModel()
        model.items = reverbItems
        var collectionView = FeatureCollectionView( headerTitle: .reverbText, model: model)
        collectionView.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            if let reverbType = action as? ReverbTypeEvent {
                self?.engineService.setVoiceReverbType(reverbType.getReverbType())
            }
        }
        return collectionView
    }()

    private lazy var advancedSettingModelArray: [AudioSettingListItemModel] = {
        var array: [AudioSettingListItemModel] = []
        let voiceEarMonitorModel = AudioSettingListItemModel()
        voiceEarMonitorModel.titleText = .voiceEarMonitorText
        voiceEarMonitorModel.hasRightSwitch = true
        voiceEarMonitorModel.hasValueLabel = false
        voiceEarMonitorModel.initSwitchState = engineService.liveKitStore.selfInfo.audioInfo.enableVoiceEarMonitor.value
        voiceEarMonitorModel.action = { [weak self] in
            guard let self = self else{ return}
            self.engineService.liveKitStore.selfInfo.audioInfo.enableVoiceEarMonitor.value =
            !self.engineService.liveKitStore.selfInfo.audioInfo.enableVoiceEarMonitor.value
            self.engineService.enableVoiceEarMonitor(enable: self.engineService.liveKitStore.selfInfo.audioInfo.enableVoiceEarMonitor.value)
        }
        array.append(voiceEarMonitorModel)
        let isEmpty = engineService.liveKitStore.currentMusicInfo.value.name.count == 0
        let dropDownText = isEmpty ? .chooseMusicText : engineService.liveKitStore.currentMusicInfo.value.name
        let backgroundMusicModel = AudioSettingListItemModel()
        backgroundMusicModel.titleText = .backgroundMusicText
        backgroundMusicModel.dropDwonText = dropDownText
        backgroundMusicModel.hasRightButton = true
        backgroundMusicModel.hasValueLabel = false
        backgroundMusicModel.hasLineView = false
        backgroundMusicModel.action = { [weak self] in
            guard let self = self else { return }
            PopupPanelController.alertView(AnchorMusicPlayPanel(engineService: self.engineService))
        }
        array.append(backgroundMusicModel)
        return array
    }()

    private lazy var audioSettingModelArray: [AudioSettingListItemModel] = {
        var array: [AudioSettingListItemModel] = []
        let musicVolumeModel = AudioSettingListItemModel()
        musicVolumeModel.titleText = .musicVolumeText
        musicVolumeModel.value = Float(engineService.liveKitStore.selfInfo.audioInfo.musicVolume.value)
        musicVolumeModel.hasSlider = true
        array.append(musicVolumeModel)

        let voiceVolumeModel = AudioSettingListItemModel()
        voiceVolumeModel.titleText = .microphoneVolumeText
        voiceVolumeModel.value = Float(engineService.liveKitStore.selfInfo.audioInfo.voiceVolume.value)
        voiceVolumeModel.hasSlider = true
        array.append(voiceVolumeModel)

        let musicPitchModel = AudioSettingListItemModel()
        musicPitchModel.titleText = .voicePitchText
        musicPitchModel.value = Float(engineService.liveKitStore.currentMusicInfo.value.pitch)
        musicPitchModel.hasSlider = true
        musicPitchModel.hasLineView = false
        musicPitchModel.minimumValue = -1.0
        musicPitchModel.maximumValue = 1.0
        array.append(musicPitchModel)
        return array
    }()

    private lazy var changerItems: [FeatureCollectionViewItem] = {
        var items: [FeatureCollectionViewItem] = []
        let changerList: [ChangerTypeEvent] = [.withoutChangerClick,
                                               .childChangerClick,
                                               .girlChangerClick,
                                               .uncleChangerClick,
                                               .etherealChangerClick,]
        let changerType = engineService.liveKitStore.selfInfo.audioInfo.changerType.value
        for changer in changerList {
            let item = FeatureCollectionViewItem(title: changer.getString(),
                                                 image: changer.getImage(),
                                                 isSelected: changer.getChangerType() == changerType,
                                                 action: changer)
            items.append(item)
        }
        return items
    }()

    private lazy var reverbItems: [FeatureCollectionViewItem] = {
        var items: [FeatureCollectionViewItem] = []
        let reverbList: [ReverbTypeEvent] = [.withoutReverbClick,
                                             .karaokeReverbClick,
                                             .metalReverbClick,
                                             .lowReverbClick,
                                             .loudReverbClick,]
        for reverb in reverbList {
            let isSelected = reverb.getReverbType() == engineService.liveKitStore.selfInfo.audioInfo.reverbType.value
            let item = FeatureCollectionViewItem(title: reverb.getString(),
                                                      image: reverb.getImage(),
                                                 isSelected: isSelected,
                                                      action: reverb)
            items.append(item)
        }
        return items
    }()

    func registerObserver() {
        engineService.liveKitStore.currentMusicInfo.addObserver(self) { [weak self] _, _ in
            guard let self = self else { return }
            let text = self.engineService.liveKitStore.currentMusicInfo.value.id != -1 ?
            self.engineService.liveKitStore.currentMusicInfo.value.name : .chooseMusicText
            self.backgroundMusicItemView?.setDropDownLabelText(text: text)
            self.musicPitchItemView?.setSliderValue(value: self.engineService.liveKitStore.currentMusicInfo.value.pitch)
        }

        self.engineService.liveKitStore.musicList.addObserver(self) { [weak self] _, _ in
            guard let self = self else{ return}
            if self.engineService.liveKitStore.musicList.value.count == 0 {
                self.backgroundMusicItemView?.setDropDownLabelText(text: .backgroundMusicText)
            }
        }
    }
}

// MARK: Layout

extension AudioEffectManagerPanel {
    func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 15
        layer.masksToBounds = true
        
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(scrollView)
        scrollView.addSubview(contentView)
        contentView.addSubview(advancedFeatureStackView)
        contentView.addSubview(audioSettingLabel)
        contentView.addSubview(audioSettingStackView)
        contentView.addSubview(changerCollectionView)
        contentView.addSubview(reverbCollectionView)

        for model in advancedSettingModelArray {
            let view = AudioSettingListItem(engineService: engineService, model: model)
            advancedFeatureStackView.addArrangedSubview(view)
            if view.titleLabel.text == .backgroundMusicText {
                backgroundMusicItemView = view
            }
            view.snp.makeConstraints { make in
                make.height.equalTo(CGFloat(56.5).scale375Height())
                make.width.equalToSuperview()
            }
        }

        for model in audioSettingModelArray {
            let view = AudioSettingListItem(engineService: engineService, model: model)
            audioSettingStackView.addArrangedSubview(view)
            if view.titleLabel.text == .voicePitchText {
                musicPitchItemView = view
            }
            view.snp.makeConstraints { make in
                make.height.equalTo(CGFloat(56.5).scale375Height())
                make.width.equalToSuperview()
            }
        }
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.width.equalToSuperview()
                make.height.equalTo(718.scale375Height())
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

        scrollView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(76.scale375Height())
            make.width.equalToSuperview()
            make.bottom.equalToSuperview()
        }

        contentView.snp.makeConstraints { make in
            make.width.equalToSuperview()
            make.height.equalToSuperview()
        }

        advancedFeatureStackView.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalToSuperview().offset(16.scale375())
            make.trailing.equalToSuperview().offset(-16.scale375())
            make.height.equalTo(112.scale375Height())
        }

        audioSettingLabel.snp.makeConstraints { make in
            make.top.equalTo(advancedFeatureStackView.snp.bottom).offset(24.scale375Height())
            make.leading.equalToSuperview().offset(16.scale375())
            make.trailing.equalToSuperview().offset(-16.scale375())
            make.height.equalTo(20.scale375Height())
        }

        audioSettingStackView.snp.makeConstraints { make in
            make.top.equalTo(audioSettingLabel.snp.bottom).offset(12.scale375Height())
            make.leading.equalToSuperview().offset(16.scale375())
            make.trailing.equalToSuperview().offset(-16.scale375())
            make.height.equalTo(168.scale375Height())
        }

        changerCollectionView.snp.makeConstraints { make in
            make.top.equalTo(audioSettingStackView.snp.bottom).offset(12.scale375Height())
            make.leading.equalToSuperview().offset(16.scale375())
            make.trailing.equalToSuperview().offset(-31.scale375())
            make.height.equalTo(112.scale375Height())
        }

        reverbCollectionView.snp.makeConstraints { make in
            make.top.equalTo(changerCollectionView.snp.bottom).offset(24.scale375Height())
            make.leading.equalToSuperview().offset(16.scale375())
            make.trailing.equalToSuperview().offset(-31.scale375())
            make.height.equalTo(112.scale375Height())
        }

        layoutIfNeeded()
        scrollView.contentSize = frame.size
    }
}

// MARK: Action

extension AudioEffectManagerPanel {
    @objc func backButtonClick() {
        popupAction?.value = .close
    }
}

extension AudioEffectManagerPanel: PopupPanelSubViewProtocol {
    func setAction(_ popupAction: Observable<PopupPanelAction>) {
        self.popupAction = popupAction
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
}

class AudioSettingListItemModel {
    var titleText: String = ""
    var value: Float = 0
    var dropDwonText: String = ""
    var minimumValue: Float = 0
    var maximumValue: Float = 100
    var action: (() -> Void)?
    var hasLineView: Bool = true
    var hasValueLabel: Bool = true
    var hasRightSwitch: Bool = false
    var hasRightButton: Bool = false
    var hasSlider: Bool = false
    var initSwitchState: Bool = false
}

class AudioSettingListItem: UIView {
    private var isViewReady = false
    private var model: AudioSettingListItemModel

    private let engineService: RoomEngineService
    init(engineService: RoomEngineService, model: AudioSettingListItemModel) {
        self.engineService = engineService
        self.model = model
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setupViewState()
        isViewReady = true
    }

    let titleLabel: UILabel = {
        let view = UILabel()
        view.font = .customFont(ofSize: 16)
        view.textColor = .whiteColor
        view.backgroundColor = .clear
        return view
    }()

    private let valueLabel: UILabel = {
        let view = UILabel()
        view.font = .customFont(ofSize: 16)
        view.textColor = .whiteColor
        view.textAlignment = .right
        view.backgroundColor = .clear
        return view
    }()

    private lazy var slider: UISlider = {
        let view = UISlider()
        view.addTarget(self, action: #selector(sliderValueChanged), for: .valueChanged)
        view.setThumbImage(.liveBundleImage("live_slider_icon"), for: .normal)
        return view
    }()

    private lazy var rightSwitch: UISwitch = {
        let view = UISwitch()
        view.isOn = true
        view.onTintColor = .b1
        view.thumbTintColor = .flowKitWhite
        view.addTarget(self, action: #selector(switchValueChanged), for: .valueChanged)
        view.setOn(model.initSwitchState, animated: true)
        return view
    }()

    private lazy var drowDownLabel: UILabel = {
        let view = UILabel()
        view.font = .customFont(ofSize: 16)
        view.textColor = .whiteColor
        view.textAlignment = .right
        view.backgroundColor = .clear
        let tap = UITapGestureRecognizer(target: self, action: #selector(dropDownLabelAction))
        view.addGestureRecognizer(tap)
        view.isUserInteractionEnabled = true
        return view
    }()

    private lazy var rightButton: UIButton = {
        let view = UIButton()
        view.setBackgroundImage(.liveBundleImage("live_drop_down_arrow"), for: .normal)
        view.addTarget(self, action: #selector(dropDownLabelAction), for: .touchUpInside)
        return view
    }()

    private let lineView: UIView = {
        let view = UIView()
        view.backgroundColor = .g6.withAlphaComponent(0.2)
        return view
    }()

    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(valueLabel)
        addSubview(slider)
        addSubview(drowDownLabel)
        addSubview(rightButton)
        addSubview(rightSwitch)
        addSubview(lineView)
    }

    func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(12.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(20.scale375Height())
            make.width.equalTo(100.scale375())
        }

        valueLabel.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-140.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(20.scale375Height())
            make.width.equalTo(45.scale375())
        }

        slider.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-23.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(14.scale375Height())
            make.width.equalTo(109.scale375())
        }

        rightSwitch.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-14.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(24.scale375Height())
            make.width.equalTo(42.scale375())
        }

        drowDownLabel.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-38.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(20.scale375Height())
            make.width.equalTo(64.scale375())
        }

        rightButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-18.scale375())
            make.centerY.equalToSuperview()
            make.height.width.equalTo(20.scale375())
        }

        lineView.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(18.scale375Height())
            make.leading.equalToSuperview().offset(16.scale375())
            make.trailing.equalToSuperview().offset(-16.scale375())
            make.height.equalTo(CGFloat(0.5).scale375Width())
        }
    }

    func setupViewState() {
        titleLabel.text = model.titleText
        let valueText = model.titleText == .voicePitchText ?
        String(format: "%.1f", model.value) : String(format: "%d", Int(model.value))
        valueLabel.text = valueText
        drowDownLabel.text = model.dropDwonText
        slider.minimumValue = model.minimumValue
        slider.maximumValue = model.maximumValue
        slider.value = model.value

        if !model.hasLineView {
            lineView.isHidden = true
        }
        if !model.hasValueLabel {
            valueLabel.isHidden = true
        }
        if !model.hasSlider {
            slider.isHidden = true
        }
        if !model.hasRightSwitch {
            rightSwitch.isHidden = true
        }
        if model.dropDwonText.isEmpty {
            drowDownLabel.isHidden = true
        }
        if !model.hasRightButton {
            rightButton.isHidden = true
        }
    }

    func setDropDownLabelText(text: String) {
        drowDownLabel.text = text
    }

    func setSliderValue(value: Float) {
        slider.setValue(value, animated: true)
        let valueText = model.titleText == .voicePitchText ?
        String(format: "%.1f", slider.value) : String(format: "%d", Int(slider.value))
        valueLabel.text = valueText
    }
}

// MARK: Action

extension AudioSettingListItem {
    @objc func sliderValueChanged() {
        switch model.titleText {
        case .musicVolumeText:
            engineService.liveKitStore.selfInfo.audioInfo.musicVolume.value = Int(slider.value)
            engineService.setMusicVolume(engineService.liveKitStore.selfInfo.audioInfo.musicVolume.value)
        case .microphoneVolumeText:
            engineService.liveKitStore.selfInfo.audioInfo.voiceVolume.value = Int(slider.value)
            engineService.setVoiceVolume(engineService.liveKitStore.selfInfo.audioInfo.voiceVolume.value)
        case .voicePitchText:
            engineService.liveKitStore.currentMusicInfo.value.pitch = Float(slider.value)
            engineService.setMusicPitch(engineService.liveKitStore.currentMusicInfo.value.id,
                                        pitch: Double(engineService.liveKitStore.currentMusicInfo.value.pitch))
        default:
            break
        }
        let valueText = model.titleText == .voicePitchText ?
        String(format: "%.1f", slider.value) : String(format: "%d", Int(slider.value))
        valueLabel.text = valueText
    }

    @objc func switchValueChanged() {
        model.action?()
    }
    
    @objc func dropDownLabelAction() {
        model.action?()
    }
}
