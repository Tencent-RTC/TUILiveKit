//
//  VideoLinkConfigPanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/10/25.
//

import Combine
import Foundation
import RTCRoomEngine
import RTCCommon
import TUICore
import LiveStreamCore

class VideoLinkSettingPanel: RTCBaseView {
    private weak var coreView: LiveCoreView?
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    private let requestTimeOutValue = 60
    
    private var cancellableSet = Set<AnyCancellable>()
    private var needCloseCameraWhenViewDisappear: Bool = false
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()
    
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
    
    private lazy var featureItems: [LSFeatureItem] = {
        var items = [LSFeatureItem]()
        var designConfig = LSFeatureItemDesignConfig()
        designConfig.imageTopInset = 8.scale375Height()
        designConfig.backgroundColor = .g3.withAlphaComponent(0.3)
        designConfig.cornerRadius = 10.scale375Width()
        designConfig.type = .imageAboveTitle
        items.append(LSFeatureItem(normalTitle: .beautyText,
                                 normalImage: .liveBundleImage("live_video_setting_beauty"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.beauty))
            let isEffectBeauty = (TUICore.getService(TUICore_TEBeautyService) != nil)
            DataReporter.reportEventData(eventKey: isEffectBeauty ? Constants.DataReport.kDataReportPanelShowLiveRoomBeautyEffect :
                                            Constants.DataReport.kDataReportPanelShowLiveRoomBeauty)
        }))
        items.append(LSFeatureItem(normalTitle: .mirrorText,
                                 normalImage: .liveBundleImage("live_video_setting_mirror"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.manager.setCameraMirror()
        }))
        items.append(LSFeatureItem(normalTitle: .flipText,
                                 normalImage: .liveBundleImage("live_video_setting_flip"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.manager.switchCamera()
        }))
        return items
    }()
    
    private lazy var featureClickPanel: LSFeatureClickPanel = {
        var model = LSFeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375Width(), height: 56.scale375Width())
        model.itemDiff = 12.scale375Width()
        model.items = featureItems
        var featureClickPanel = LSFeatureClickPanel(model: model)
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
    
    init(manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) {
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
        super.init(frame: .zero)
    }
    
    override func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(previewView)
        addSubview(featureClickPanel)
        addSubview(requestLinkMicButton)
        addSubview(tipsLabel)
    }
    
    override func activateConstraints() {
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
            make.centerX.equalToSuperview()
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
            make.bottom.equalToSuperview()
        }
    }
    
    override func bindInteraction() {
        subscribeCurrentRoute()
    }
    
    override func setupViewStyle() {
        backgroundColor = .g2
        layer.cornerRadius = 20
        layer.masksToBounds = true
    }
}

// MARK: Action
extension VideoLinkSettingPanel {
    @objc func requestLinkMicButtonClick(_ sender: LSFeatureItemButton) {
        manager.update(coGuestStatus: .applying)
        coreView?.requestIntraRoomConnection(userId: "", timeOut: requestTimeOutValue, openCamera: true) {
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            self.manager.update(coGuestStatus: .none)
            let error = InternalError(error: code, message: message)
            self.manager.toastSubject.send(error.localizedMessage)
        }
        routerManager.router(action: .routeTo(.audience))
    }
    
    private func subscribeCurrentRoute() {
        routerManager.subscribeRouterState(StateSelector(keyPath: \LSRouterState.routeStack))
            .receive(on: RunLoop.main)
            .sink { [weak self] routeStack in
                guard let self = self else { return }
                if routeStack.last == .linkSetting {
                    manager.setLocalVideoView(previewView)
                    if !manager.mediaState.isCameraOpened {
                        manager.openLocalCamera()
                        needCloseCameraWhenViewDisappear = true
                    }
                } else if !routeStack.contains(.linkSetting) {
                    if needCloseCameraWhenViewDisappear {
                        manager.closeLocalCamera()
                        needCloseCameraWhenViewDisappear = false
                    }
                }
            }
            .store(in: &cancellableSet)
    }
}

private extension String {
    static let videoLinkConfigTitleText = localized("live.audience.videoLinkConfig.title")
    static let requestText = localized("live.audience.videoLinkConfig.request")
    static let videoLinkConfigTipsText = localized("live.audience.videoLinkConfig.tips")
    static let beautyText = localized("live.audience.videoLinkConfig.beauty")
    static let makeupText = localized("live.audience.videoLinkConfig.makeup")
    static let filterText = localized("live.audience.videoLinkConfig.filter")
    static let mirrorText = localized("live.audience.videoLinkConfig.mirror")
    static let flipText = localized("live.audience.videoLinkConfig.flip")
    static let operateFailedText = localized("live.operation.fail.xxx")
}
