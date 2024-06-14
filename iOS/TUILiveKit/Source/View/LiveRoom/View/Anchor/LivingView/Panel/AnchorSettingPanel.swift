//
//  AnchorSettingPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/15.
//

import Combine
import Foundation
class AnchorSettingPanel: UIView {
    @Injected private var viewStore: LiveRoomViewStore
    @Injected private var store: LiveStore
    private var cancellableSet = Set<AnyCancellable>()
    weak var rootController: UIViewController?
    init() {
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setView()
        isViewReady = true
    }

    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .settingTitleText
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .center
        return view
    }()

    private lazy var featureItems: [FeatureItem] = {
        var items: [FeatureItem] = []
        let designConfig = FeatureItemDesignConfig()
        designConfig.backgroundColor = .g3
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.type = .imageAboveTitleBottom
        items.append(FeatureItem(title: .beautyText,
                                 image: .liveBundleImage("live_videoSetting_beauty"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] in
                                     guard let self = self else { return }
                                     self.beautyClick()
                                 }))
        items.append(FeatureItem(title: .audioEffectsText,
                                 image: .liveBundleImage("live_setting_audio_effects"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] in
            guard let self = self else { return }
            self.audioEffectClick()
        }))
        items.append(FeatureItem(title: .flipText,
                                 image: .liveBundleImage("live_videoSetting_flip"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] in
            guard let self = self else { return }
            self.flipClick()
        }))
        items.append(FeatureItem(title: .mirrorText,
                                 image: .liveBundleImage("live_videoSetting_mirror"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] in
            guard let self = self else { return }
            self.mirrorClick()
        }))
        items.append(FeatureItem(title: .videoParametersText,
                                 image: .liveBundleImage("live_setting_video_parameters"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] in
            guard let self = self else { return }
            self.videoParametersClick()
        }))
        return items
    }()

    private lazy var featureClickPanel: FeatureClickPanel = {
        let model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 76.scale375())
        model.itemDiff = 12.scale375()
        model.items = featureItems
        let featureClickPanel = FeatureClickPanel(model: model)
        return featureClickPanel
    }()

    private func setView() {
        backgroundColor = .g2
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
                make.height.equalTo(350.scale375Height())
            } else {
                make.width.equalTo(375.scale375())
            }
            make.edges.equalToSuperview()
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
        viewStore.navigate(action: .present(.beauty(false)))
    }

    private func audioEffectClick() {
        viewStore.navigate(action: .present(.audioEffect))
    }

    private func mirrorClick() {
        let isMirror = store.selectCurrent(MediaSelectors.getMirrorState)
        store.dispatch(action: MediaActions.switchMirror(payload: isMirror == true ? false : true))
    }

    private func flipClick() {
        let isFrontCamera = store.selectCurrent(MediaSelectors.getFrontCameraState)
        store.dispatch(action: MediaActions.switchCamera(payload: isFrontCamera == true ? .rear : .front))
    }

    private func moreSettingClick() {
    }

    private func videoParametersClick() {
        viewStore.navigate(action: .present(.videoSetting))
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
