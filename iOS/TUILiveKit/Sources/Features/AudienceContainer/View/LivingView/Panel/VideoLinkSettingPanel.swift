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
import AtomicXCore

class VideoLinkSettingPanel: RTCBaseView {
    private weak var coreView: LiveCoreView?
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
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
    
    private lazy var featureItems: [AudienceFeatureItem] = {
        var items = [AudienceFeatureItem]()
        var designConfig = AudienceFeatureItemDesignConfig()
        designConfig.imageTopInset = 8.scale375Height()
        designConfig.backgroundColor = .g3.withAlphaComponent(0.3)
        designConfig.cornerRadius = 10.scale375Width()
        designConfig.type = .imageAboveTitle
        items.append(AudienceFeatureItem(normalTitle: .beautyText,
                                 normalImage: internalImage("live_video_setting_beauty"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.beauty))
            let isEffectBeauty = (TUICore.getService(TUICore_TEBeautyService) != nil)
            DataReporter.reportEventData(eventKey: isEffectBeauty ? Constants.DataReport.kDataReportPanelShowLiveRoomBeautyEffect :
                                            Constants.DataReport.kDataReportPanelShowLiveRoomBeauty)
        }))
        items.append(AudienceFeatureItem(normalTitle: .flipText,
                                 normalImage: internalImage("live_video_setting_flip"),
                                 designConfig: designConfig,
                                 actionClosure: { [weak self] _ in
            guard let self = self else { return }
            coreView?.switchCamera(isFront: !manager.coreMediaState.isFrontCamera)
        }))
        return items
    }()
    
    private lazy var featureClickPanel: AudienceFeatureClickPanel = {
        var model = AudienceFeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375Width(), height: 56.scale375Width())
        model.itemDiff = 12.scale375Width()
        model.items = featureItems
        var featureClickPanel = AudienceFeatureClickPanel(model: model)
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
    
    init(manager: AudienceManager, routerManager: AudienceRouterManager, coreView: LiveCoreView) {
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
    @objc func requestLinkMicButtonClick(_ sender: AudienceFeatureItemButton) {
        manager.onStartRequestIntraRoomConnection()
        coreView?.requestIntraRoomConnection(userId: "", timeOut: requestTimeOutValue, openCamera: true) { [weak self] in
            guard let self = self else { return }
            manager.toastSubject.send(.waitToLinkText)
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            manager.onRequestIntraRoomConnectionFailed()
            let error = InternalError(code: code.rawValue, message: message)
            manager.onError(error)
        }
        routerManager.router(action: .routeTo(.audience))
    }
    
    private func subscribeCurrentRoute() {
        routerManager.subscribeRouterState(StateSelector(keyPath: \AudienceRouterState.routeStack))
            .receive(on: RunLoop.main)
            .sink { [weak self] routeStack in
                guard let self = self else { return }
                if routeStack.last == .linkSetting {
                    guard let coreView = coreView else { return }
                    if !manager.coreMediaState.isCameraOpened {
                        coreView.startCamera(useFrontCamera: manager.coreMediaState.isFrontCamera) { [weak self] in
                            guard let self = self else { return }
                            manager.onCameraOpened(localVideoView: previewView)
                            needCloseCameraWhenViewDisappear = true
                        } onError: { [weak self] code, msg in
                            guard let self = self else { return }
                            let error = InternalError(code: code.rawValue, message: msg)
                            manager.onError(error)
                        }
                    }
                } else if !routeStack.contains(.linkSetting) {
                    if needCloseCameraWhenViewDisappear {
                        coreView?.stopCamera()
                        needCloseCameraWhenViewDisappear = false
                    }
                }
            }
            .store(in: &cancellableSet)
    }
}

private extension String {
    static let videoLinkConfigTitleText = internalLocalized("Adjust the video link screen")
    static let requestText = internalLocalized("Apply for link mic")
    static let videoLinkConfigTipsText = internalLocalized("The screen effect will automatically take effect after connecting")
    static let beautyText = internalLocalized("Beauty")
    static let videoParametersText = internalLocalized("Video Config")
    static let flipText = internalLocalized("Flip")
    static let waitToLinkText = internalLocalized("You have submitted a link mic request, please wait for the author approval")
}
