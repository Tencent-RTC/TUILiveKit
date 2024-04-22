//
//  AnchorSettingPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/15.
//

import Foundation

class AnchorSettingPanel: UIView {
    weak var rootController: UIViewController?
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

    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        setView()
        backgroundColor = .g2
    }

    private var popupAction: Observable<PopupPanelAction>?
    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .settingTitleText
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .center
        return view
    }()
    
    private var featureItems : [FeatureItem] {
        var items:[FeatureItem] = []
        let designConfig = FeatureItemDesignConfig()
        designConfig.backgroundColor = .g3
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.type = .imageAboveTitleBottom
        items.append(FeatureItem(title: .beautyText,
                                       image: .liveBundleImage("live_videoSetting_beauty"),
                                       designConfig: designConfig,
                                       action: AnchorSettingActionEvent.beautyClick))
        items.append(FeatureItem(title: .audioEffectsText,
                                       image: .liveBundleImage("live_setting_audio_effects"),
                                       designConfig: designConfig,
                                       action: AnchorSettingActionEvent.audioEffectsClick))
        items.append(FeatureItem(title: .flipText,
                                       image: .liveBundleImage("live_videoSetting_flip"),
                                       designConfig: designConfig,
                                       action: AnchorSettingActionEvent.flipClick))
        items.append(FeatureItem(title: .mirrorText,
                                       image: .liveBundleImage("live_videoSetting_mirror"),
                                       designConfig: designConfig,
                                       action: AnchorSettingActionEvent.mirrorClick))
        items.append(FeatureItem(title: .videoParametersText,
                                       image: .liveBundleImage("live_setting_video_parameters"),
                                       designConfig: designConfig,
                                       action: AnchorSettingActionEvent.videoParametersClick))
        return items
    }
    
    private lazy var featureClickPanel: FeatureClickPanel = {
        var model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 76.scale375())
        model.itemDiff = 12.scale375()
        model.items = featureItems
        var featureClickPanel = FeatureClickPanel(model: model)
        featureClickPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            guard let self = self else{ return}
            guard let action = action as? AnchorSettingActionEvent else{ return}
            switch action {
            case .beautyClick:
                self.beautyClick()
            case .audioEffectsClick:
                self.audioEffectClick()
            case .flipClick:
                self.flipClick()
            case .mirrorClick:
                self.mirrorClick()
            case .videoParametersClick:
                self.videoParametersClick()
            default: break
            }
        }
        return featureClickPanel
    }()
    

    private func setView() {
        backgroundColor = .b2d
        layer.cornerRadius = 20
        layer.masksToBounds = true
    }
}

// MARK: Layout

private extension AnchorSettingPanel {
    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(featureClickPanel)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.width.equalToSuperview()
                make.height.equalTo(350.scale375Height())
                make.centerX.equalToSuperview()
            } else {
                make.width.equalTo(375)
                make.height.equalToSuperview()
                make.trailing.equalToSuperview()
            }
            make.bottom.equalToSuperview()
        }

        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }

        featureClickPanel.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(32.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalTo(featureClickPanel.mm_w)
            make.height.equalTo(featureClickPanel.mm_h)
        }

    }
}

// MARK: Action

private extension AnchorSettingPanel {
    private func beautyClick() {
        PopupPanelController.alertView(BeautyPanel(engineService: engineService, hasRenderView: false))
    }
    
    private func audioEffectClick() {
        let view = AudioEffectView(frame: .zero)
        let menuContainerView = MenuContainerView(contentView: view)
        let viewController = PopupViewController(contentView: menuContainerView)
        menuContainerView.blackAreaClickClosure = { [weak viewController] in
            viewController?.dismiss(animated: true)
        }
        view.backButtonClickClosure = { [weak viewController] _ in
            viewController?.dismiss(animated: true)
        }
        WindowUtils.getCurrentWindowViewController()?.present(viewController, animated: true)
    }
    
    private func mirrorClick() {
        engineService.switchMirror()
    }

    private func flipClick() {
        engineService.switchCamera()
    }

    private func moreSettingClick() {
        PopupPanelController.alertView(MoreSettingsPanel(list: liveRoomInfo.roomConfig.getListConfigModel()))
    }
    
    private func videoParametersClick() {
        PopupPanelController.alertView(AnchorVideoParametersSettingPanel(engineService: engineService))
    }
    
}

extension AnchorSettingPanel: PopupPanelSubViewProtocol {
    func setAction(_ action: Observable<PopupPanelAction>) {
        popupAction = action
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
}

private extension String {
    static var settingTitleText: String {
        localized("live.anchor.setting.title")
    }

    static var beautyText: String {
        localized("live.anchor.setting.beauty")
    }

    static var audioEffectsText: String {
        localized("live.anchor.setting.audio.effects")
    }

    static var flipText: String {
        localized("live.anchor.setting.flip")
    }

    static var mirrorText: String {
        localized("live.anchor.setting.mirror")
    }

    static var videoParametersText: String {
        localized("live.anchor.setting.video.parameters")
    }

    static var moreSettingText: String {
        localized("live.anchor.setting.more.setting")
    }
}
