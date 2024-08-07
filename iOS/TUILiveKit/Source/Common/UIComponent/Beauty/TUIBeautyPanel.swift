//
//  TUIBeautyPanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/7/4.
//

import UIKit
import Combine
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif
import TUICore

let TUICore_TEBeautyExtension_GetBeautyPanel = "TUICore_TEBeautyExtension_GetBeautyPanel"
let TUICore_TEBeautyService = "TUICore_TEBeautyService"
let TUICore_TEBeautyService_SetLicense = "TUICore_TEBeautyService_SetLicense"
let TUICore_TEBeautyService_LicenseUrl = "TUICore_TEBeautyService_LicenseUrl"
let TUICore_TEBeautyService_LicenseKey = "TUICore_TEBeautyService_LicenseKey"
let TUICore_TEBeautyService_ProcessVideoFrameWithTexture = "TUICore_TEBeautyService_ProcessVideoFrameWithTexture"
let TUICore_TEBeautyService_ProcessVideoFrame_TextureId = "TUICore_TEBeautyService_ProcessVideoFrame_TextureId"
let TUICore_TEBeautyService_ProcessVideoFrame_TextureWidth = "TUICore_TEBeautyService_ProcessVideoFrame_TextureWidth"
let TUICore_TEBeautyService_ProcessVideoFrame_TextureHeight = "TUICore_TEBeautyService_ProcessVideoFrame_TextureHeight"
let TUICore_TEBeautyService_ProcessVideoFrameWithPixelData = "TUICore_TEBeautyService_ProcessVideoFrameWithPixelData"
let TUICore_TEBeautyService_ProcessVideoFrame_PixelValue = "TUICore_TEBeautyService_ProcessVideoFrame_PixelValue"
let TUICore_TEBeautyService_SetPanelLevel = "TUICore_TEBeautyService_SetPanelLevel"
let TUICore_TEBeautyService_PanelLevel = "TUICore_TEBeautyService_PanelLevel"

class TUIBeautyPanel: UIView, TRTCCloudDelegate {
    private var isAdvancedBeauty = false
    private let store: LiveStoreProvider
    private let roomEngine: TUIRoomEngine
    private lazy var trtcCloud = roomEngine.getTRTCCloud()
    private var routerStore: RouterStore
    private var cancellableSet = Set<AnyCancellable>()
    private var beautyPanel: UIView = UIView()
    
    init(store: LiveStoreProvider, routerStore: RouterStore) {
        self.store = store
        self.roomEngine = self.store.roomEngine
        self.routerStore = routerStore
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        if let advancedBeautyPanel = getAdvancedBeautyPanel() {
            beautyPanel = advancedBeautyPanel
            beautyPanel.backgroundColor = .black
            setBeautyMode(isAdvanced: true)
        } else {
            let isOwner = store.selectCurrent(UserSelectors.isOwner)
            beautyPanel = BeautyPanel(hasRenderView: !isOwner, store: store, routerStore: routerStore)
            setBeautyMode(isAdvanced: false)
        }
        addSubview(beautyPanel)
    }
    
    private func activateConstraints() {
        beautyPanel.snp.makeConstraints { [weak self] make in
            guard let self = self else { return }
            make.edges.equalToSuperview()
            make.height.equalTo(isAdvancedBeauty ? 200.scale375Height() : 374.scale375Height())
        }
    }
    
    private func bindInteraction() {
        addObserver()
        guard !isAdvancedBeauty, let panel = beautyPanel as? BeautyPanel else { return }
        panel.backClosure = { [weak self] in
            guard let self = self else { return }
            self.routerStore.router(action: .dismiss())
        }
    }
    
    private func getAdvancedBeautyPanel() -> UIView? {
        let beautyPanelList = TUICore.getExtensionList(TUICore_TEBeautyExtension_GetBeautyPanel,
                                                       param: ["width":375.scale375(),
                                                               "height":200.scale375Height(),])
        if beautyPanelList.count == 0 {
            return nil
        }
        guard let beautyPanel = beautyPanelList.first?.data?["TUICore_TEBeautyExtension_GetBeautyPanel"] as? UIView else { return nil }
        return beautyPanel
    }
    
    private func setBeautyMode(isAdvanced: Bool) {
        isAdvancedBeauty = isAdvanced
    }
    
    private func addObserver() {
        if !isAdvancedBeauty { return }
        trtcCloud.addDelegate(self)
        trtcCloud.setLocalVideoProcessDelegete(self, pixelFormat: ._NV12, bufferType: .pixelBuffer)
    }
    
    private func removeObserver() {
        if !isAdvancedBeauty { return }
        trtcCloud.setLocalVideoProcessDelegete(nil, pixelFormat: ._NV12, bufferType: .pixelBuffer)
    }
    
    deinit {
        removeObserver()
        debugPrint("\(type(of: self)) deinit")
    }
}

// MARK: Action
extension TUIBeautyPanel {
    @objc func backButtonClick(sender: UIButton) {
        routerStore.router(action: .dismiss())
    }
}

// MARK: TRTCVideoFrameDelegate

extension TUIBeautyPanel: TRTCVideoFrameDelegate {
    func onProcessVideoFrame(_ srcFrame: TRTCVideoFrame, dstFrame: TRTCVideoFrame) -> UInt32 {
        guard let pixelBuffer = srcFrame.pixelBuffer else { return 0 }
        let pixelBufferValue = NSValue(pointer: Unmanaged.passUnretained(pixelBuffer).toOpaque())
        if let resultPixelBufferValue = TUICore.callService(TUICore_TEBeautyService, 
                                                            method: TUICore_TEBeautyService_ProcessVideoFrameWithPixelData,
                                            param: [TUICore_TEBeautyService_ProcessVideoFrame_PixelValue: pixelBufferValue,])
        as? NSValue {
            guard let pointerValue = resultPixelBufferValue.pointerValue else { return 0 }
            let resultPixelBuffer = Unmanaged<CVPixelBuffer>.fromOpaque(pointerValue).takeUnretainedValue()
            dstFrame.pixelBuffer = resultPixelBuffer
        }
        return 0
    }
}
