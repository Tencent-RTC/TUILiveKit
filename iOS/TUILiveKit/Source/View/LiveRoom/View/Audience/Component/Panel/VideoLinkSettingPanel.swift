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

class VideoLinkSettingPanel: RTCBaseView {
    @Injected private var store: LiveStore
    @Injected private var viewStore: LiveRoomViewStore
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
    
    private lazy var featureItems: [FeatureItem] = {
        var items = [FeatureItem]()
        let designConfig = FeatureItemDesignConfig()
        designConfig.imageTopInset = 8.scale375Height()
        designConfig.backgroundColor = .g3.withAlphaComponent(0.3)
        designConfig.cornerRadius = 10.scale375Width()
        designConfig.type = .imageAboveTitle
        items.append(FeatureItem(title: .beautyText, image: .liveBundleImage("live_videoSetting_beauty"),
                                 designConfig: designConfig, actionClosure: { [weak self] in
            guard let self = self else { return }
            self.viewStore.navigate(action: .present(.beauty(true)))
        }))
        items.append(FeatureItem(title: .mirrorText, image: .liveBundleImage("live_videoSetting_mirror"),
                                 designConfig: designConfig, actionClosure: { [weak self] in
            guard let self = self else { return }
            let isMirror = store.selectCurrent(MediaSelectors.getMirrorState)
            self.store.dispatch(action: MediaActions.switchMirror(payload: isMirror == true ? false : true))
        }))
        items.append(FeatureItem(title: .flipText, image: .liveBundleImage("live_videoSetting_flip"),
                                 designConfig: designConfig, actionClosure: { [weak self] in
            guard let self = self else { return }
            let isFrontCamera = store.selectCurrent(MediaSelectors.getFrontCameraState)
            store.dispatch(action: MediaActions.switchCamera(payload: isFrontCamera == true ? .rear : .front))
        }))
        return items
    }()
    
    private lazy var featureClickPanel: FeatureClickPanel = {
        var model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375Width(), height: 56.scale375Width())
        model.itemDiff = 12.scale375Width()
        model.items = featureItems
        var featureClickPanel = FeatureClickPanel(model: model)
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
    @objc func requestLinkMicButtonClick(_ sender: FeatureItemButton) {
        store.dispatch(action: ViewActions.updateAutoOpenCameraOnSeated(payload: true))
        store.dispatch(action: SeatActions.takeSeat(payload: nil))
        viewStore.navigate(action: .popToRoute(.audience))
    }
    
    private func subscribeCurrentRoute() {
        viewStore.select(LiveRoomViewSelectors.routeStack)
            .receive(on: RunLoop.main)
            .sink { [weak self] routeStack in
                guard let self = self else { return }
                if routeStack.last == .linkSetting {
                    store.dispatch(action: MediaActions.updateLocalVideoView(payload: previewView))
                    if !store.mediaState.isCameraOpened {
                        store.dispatch(action: MediaActions.operateCamera(payload: true))
                        needCloseCameraWhenViewDisappear = true
                    }
                } else if !routeStack.contains(.linkSetting) {
                    if needCloseCameraWhenViewDisappear {
                        store.dispatch(action: MediaActions.operateCamera(payload: false))
                        needCloseCameraWhenViewDisappear = false
                    }
                }
            }
            .store(in: &cancellableSet)
    }
}

private extension String {
    static var videoLinkConfigTitleText: String =
        localized("live.audience.videoLinkConfig.title")
    static let requestText = localized("live.audience.videoLinkConfig.request")
    static let videoLinkConfigTipsText = localized("live.audience.videoLinkConfig.tips")
    static let beautyText = localized("live.audience.videoLinkConfig.beauty")
    static let makeupText = localized("live.audience.videoLinkConfig.makeup")
    static let filterText = localized("live.audience.videoLinkConfig.filter")
    static let mirrorText = localized("live.audience.videoLinkConfig.mirror")
    static let flipText = localized("live.audience.videoLinkConfig.flip")
    static let operateFailedText = localized("live.operation.fail.xxx")
}
