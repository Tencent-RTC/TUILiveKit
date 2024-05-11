//
//  AnchorVideoParametersSettingPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/15.
//

import Foundation
import RTCRoomEngine

class AnchorVideoParametersSettingPanel: UIView {
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

    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .settingTitleText
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .center
        return view
    }()

    private lazy var videoQualitySelectionModel: DropDownArrowModel = {
        let model = DropDownArrowModel()
        let str = engineService.liveRoomInfo.selfInfo.videoInfo.videoQuality.value.getString()
        model.midText.value = str
        model.rightIcon = .liveBundleImage("live_drop_down_arrow")
        engineService.liveRoomInfo.selfInfo.videoInfo.videoQuality.addObserver(self) { [weak self] _, _ in
            guard let self = self else { return }
            let str = engineService.liveRoomInfo.selfInfo.videoInfo.videoQuality.value.getString()
            self.videoQualitySelectionModel.midText.value = str
        }
        return model
    }()

    private let videoQualityTitleLabel: UILabel = {
        let view = UILabel()
        view.text = .sharpnessText
        view.textColor = .g5
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .left
        return view
    }()

    private lazy var videoQualityButton: DropDownArrowButton = {
        let view = DropDownArrowButton(model: videoQualitySelectionModel)
        view.addTarget(self, action: #selector(videoQualityButtonClick), for: .touchUpInside)
        return view
    }()

    private func setView() {
        backgroundColor = .b2d
        layer.cornerRadius = 20
        layer.masksToBounds = true
    }

}

// MARK: Layout

extension AnchorVideoParametersSettingPanel {
    func constructViewHierarchy() {
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(videoQualityTitleLabel)
        addSubview(videoQualityButton)
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

        backButton.snp.remakeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }

        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }

        videoQualityButton.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(32.scale375Height())
            make.trailing.equalToSuperview().inset(24)
            make.height.equalTo(20.scale375Height())
        }

        videoQualityTitleLabel.snp.makeConstraints { make in
            make.top.centerY.equalTo(videoQualityButton)
            make.leading.equalToSuperview().inset(24)
            make.height.equalTo(22.scale375Height())
        }
    }
}

// MARK: Action

extension AnchorVideoParametersSettingPanel {
    @objc func backButtonClick() {
        popupAction?.value = .close
    }

    @objc func videoQualityButtonClick() {
        let model = ActionModel()
        let designConfig = ActionItemDesignConfig()
        var config: ActionItemDesignConfig
        let categoryList: [TUIVideoQuality] = [.quality360P, .quality540P, .quality720P, .quality1080P]
        for category in categoryList {
            if category == categoryList.last {
                config = ActionItemDesignConfig(lineWidth: 7)
            } else {
                config = designConfig
            }
            let item = ActionItem(text: category.getString(), designConfig: config, action: PrepareViewActionEvent.videoQualityValue(category))
            model.items.append(item)
        }
        
        let actionPanel = ActionPanel(model: model)
        PopupPanelController.alertView(actionPanel)
        actionPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            guard let actionType = action as?  PrepareViewActionEvent else{ return}
            if case let .videoQualityValue(value) = actionType {
                self?.engineService.liveRoomInfo.selfInfo.videoInfo.videoQuality.value = value
                self?.engineService.updateVideoQuality(quality: value)
            }
        }
    }
}

extension AnchorVideoParametersSettingPanel: PopupPanelSubViewProtocol {
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
        localized("live.anchor.setting.video.parameters")
    }

    static var sharpnessText: String {
        localized("live.anchor.setting.video.sharpness")
    }
}
