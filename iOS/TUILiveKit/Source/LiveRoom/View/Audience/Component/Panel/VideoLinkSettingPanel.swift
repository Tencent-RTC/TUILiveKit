//
//  VideoLinkConfigPanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/10/25.
//

import Foundation
import RTCRoomEngine

class VideoLinkSettingPanel: UIView {
    static var shouldUpdateSelf: Observable<Bool> = Observable(false)
    let clickEventCallBack: Observable<Any> = Observable(LiveKitClickEvent.default)
    private var liveRoomInfo:LiveRoomInfo {
        engineService.liveRoomInfo
    }
    private let engineService: RoomEngineService
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
        addObserver()
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
        constructViewHierarchy()
        activateConstraints()
        setView()
        backgroundColor = .g2
        isViewReady = true
    }
    
    private var action: Observable<PopupPanelAction>?

    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .videoLinkConfigTitleText
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .center
        return view
    }()

    private let previewView: UIView = {
        let view = UIView()
        view.layer.cornerRadius = 16.scale375Width()
        view.layer.masksToBounds = true
        return view
    }()

    private var featureItems: [FeatureItem] = {
        var items = [FeatureItem]()
        let designConfig = FeatureItemDesignConfig()
        designConfig.backgroundColor = .g3
        designConfig.cornerRadius = 10.scale375Width()
        designConfig.type = .imageAboveTitle
        items.append(FeatureItem(title: .beautyText, image: .liveBundleImage("live_videoSetting_beauty"),
                                       designConfig: designConfig, action: AudienceVideoSettingActionEvent.beautyClick))
        items.append(FeatureItem(title: .mirrorText, image: .liveBundleImage("live_videoSetting_mirror"),
                                       designConfig: designConfig, action: AudienceVideoSettingActionEvent.mirrorClick))
        items.append(FeatureItem(title: .flipText, image: .liveBundleImage("live_videoSetting_flip"),
                                       designConfig: designConfig, action: AudienceVideoSettingActionEvent.flipClick))
        return items
    }()
    
    private lazy var featureClickPanel: FeatureClickPanel = {
        var model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375Width(), height: 56.scale375Width())
        model.itemDiff = 12.scale375Width()
        model.items = featureItems
        var featureClickPanel = FeatureClickPanel(model: model)
        featureClickPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            guard let self = self else{ return}
            guard let actionType = action as? AudienceVideoSettingActionEvent else{ return}
            switch actionType {
            case .beautyClick:
                PopupPanelController.alertView(BeautyPanel(engineService: self.engineService))
            case .mirrorClick:
                self.engineService.switchMirror()
            case .flipClick:
                self.engineService.switchCamera()
            default:
                break
            }
        }

        return featureClickPanel
    }()

    private lazy var requestLinkMicButton: UIButton = {
        let view = UIButton()
        view.setTitle(.requestText, for: .normal)
        view.setTitleColor(.flowKitWhite, for: .normal)
        view.titleLabel?.font = .customFont(ofSize: 16)
        view.backgroundColor = .brandBlueColor
        view.layer.cornerRadius = 10.scale375Width()
        view.layer.masksToBounds = true
        view.addTarget(self, action: #selector(requestLinkMicButtonClick), for: .touchUpInside)
        return view
    }()

    private let tipsLabel: UILabel = {
        let view = UILabel()
        view.text = .videoLinkConfigTipsText
        view.textColor = .greyColor
        view.font = .customFont(ofSize: 12)
        view.textAlignment = .center
        view.numberOfLines = 0
        view.lineBreakMode = .byWordWrapping
        return view
    }()

    private func setView() {
        backgroundColor = .b2d
        layer.cornerRadius = 20
        layer.masksToBounds = true

        engineService.setLocalVideoView(view: previewView)
        engineService.openLocalCamera() {
        } onError: { [weak self] code, message in
            let statusString = String(code.rawValue) + "," + message
            self?.makeToast(.localizedReplace(.operateFailedText, replace: statusString))
        }
    }
    
    func addObserver() {
        VideoLinkSettingPanel.shouldUpdateSelf.addObserver(self) { [weak self] shouldUpdate, _ in
            if shouldUpdate {
                guard let self = self else { return }
                self.engineService.setLocalVideoView(view: previewView)
            }
        }
    }
    
    func clearSource() {
        VideoLinkSettingPanel.shouldUpdateSelf.removeObserver(self)
        engineService.closeLocalCamera()
    }
    
    deinit {
        clearSource()
    }
}

// MARK: Layout

extension VideoLinkSettingPanel {
    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(previewView)
        addSubview(featureClickPanel)
        addSubview(requestLinkMicButton)
        addSubview(tipsLabel)
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

        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }

        previewView.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(32.scale375Height())
            make.leading.equalToSuperview().offset(24.scale375Width())
            make.trailing.equalToSuperview().offset(-23.scale375Width())
            make.height.equalTo(328.scale375Height())
        }

        featureClickPanel.snp.makeConstraints { make in
            make.top.equalTo(previewView.snp.bottom).offset(24.scale375Height())
            make.leading.equalTo(previewView)
            make.trailing.equalTo(previewView)
            make.height.equalTo(56.scale375Width())
        }

        requestLinkMicButton.snp.makeConstraints { make in
            make.top.equalTo(featureClickPanel.snp.bottom).offset(111.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalTo(200.scale375Width())
            make.height.equalTo(52.scale375Height())
        }

        tipsLabel.snp.makeConstraints { make in
            make.top.equalTo(requestLinkMicButton.snp.bottom).offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(17.scale375Height())
        }
    }
}

// MARK: Action

extension VideoLinkSettingPanel {
    @objc func requestLinkMicButtonClick(_ sender: FeatureItemButton) {
        action?.value = .close
        clickEventCallBack.value = AudienceLinkActionEvent.videoLinkClick
    }
}

extension VideoLinkSettingPanel: PopupPanelSubViewProtocol {
    func setAction(_ action: Observable<PopupPanelAction>) {
        self.action = action
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
}

private extension String {
    static var videoLinkConfigTitleText: String {
        localized("live.audience.videoLinkConfig.title")
    }

    static var requestText: String {
        localized("live.audience.videoLinkConfig.request")
    }

    static var videoLinkConfigTipsText: String {
        localized("live.audience.videoLinkConfig.tips")
    }

    static var beautyText: String {
        localized("live.audience.videoLinkConfig.beauty")
    }

    static var makeupText: String {
        localized("live.audience.videoLinkConfig.makeup")
    }

    static var filterText: String {
        localized("live.audience.videoLinkConfig.filter")
    }

    static var mirrorText: String {
        localized("live.audience.videoLinkConfig.mirror")
    }

    static var flipText: String {
        localized("live.audience.videoLinkConfig.flip")
    }
    
    static var operateFailedText: String {
        localized("live.operation.fail.xxx")
    }
}
