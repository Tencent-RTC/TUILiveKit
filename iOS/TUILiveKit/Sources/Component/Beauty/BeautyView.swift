//
//  BeautyView.swift
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
import RTCCommon

let TUICore_TEBeautyExtension_GetBeautyPanel = "TUICore_TEBeautyExtension_GetBeautyPanel"
let TUICore_TEBeautyService = "TUICore_TEBeautyService"
let TUICore_TEBeautyService_SetLicense = "TUICore_TEBeautyService_SetLicense"
let TUICore_TEBeautyService_LicenseUrl = "TUICore_TEBeautyService_LicenseUrl"
let TUICore_TEBeautyService_LicenseKey = "TUICore_TEBeautyService_LicenseKey"
let TUICore_TEBeautyService_CheckResource = "TUICore_TEBeautyService_CheckResource"
let TUICore_TEBeautyService_ProcessVideoFrameWithTexture = "TUICore_TEBeautyService_ProcessVideoFrameWithTexture"
let TUICore_TEBeautyService_ProcessVideoFrame_TextureId = "TUICore_TEBeautyService_ProcessVideoFrame_TextureId"
let TUICore_TEBeautyService_ProcessVideoFrame_TextureWidth = "TUICore_TEBeautyService_ProcessVideoFrame_TextureWidth"
let TUICore_TEBeautyService_ProcessVideoFrame_TextureHeight = "TUICore_TEBeautyService_ProcessVideoFrame_TextureHeight"
let TUICore_TEBeautyService_ProcessVideoFrameWithPixelData = "TUICore_TEBeautyService_ProcessVideoFrameWithPixelData"
let TUICore_TEBeautyService_ProcessVideoFrame_PixelValue = "TUICore_TEBeautyService_ProcessVideoFrame_PixelValue"
let TUICore_TEBeautyService_SetPanelLevel = "TUICore_TEBeautyService_SetPanelLevel"
let TUICore_TEBeautyService_PanelLevel = "TUICore_TEBeautyService_PanelLevel"
let TUICore_TEBeautyService_CachedBeautyEffect = "TUICore_TEBeautyService_CachedBeautyEffect"

class BeautyView: UIView {
    static func shared() -> BeautyView {
        if let instance = instance {
            return instance
        }
        let view = BeautyView()
        instance = view
        return view
    }
    private static var instance: BeautyView?
    
    var backClosure: (()->Void)?
    var resetBeautyEffectForAdvancedBeauty = true
    private var isAdvancedBeauty = false
    private let roomEngine: TUIRoomEngine
    private lazy var trtcCloud = roomEngine.getTRTCCloud()
    private var cancellableSet = Set<AnyCancellable>()
    private var beautyPanel: UIView = UIView()
    
    private let Screen_Width = UIScreen.main.bounds.size.width
    private let Screen_Height = UIScreen.main.bounds.size.height
    
    private static let stateKey = "__kBeautyView_state_key__"
    
    private init() {
        roomEngine = TUIRoomEngine.sharedInstance()
        super.init(frame: .zero)
        subscribe()
    }
    
    private func subscribe() {
        StateCache.shared.subscribeToObjectRemoval(key: BeautyView.stateKey) {
            BeautyView.instance = nil
            DispatchQueue.main.async {
                BeautyView.shared().subscribe()
            }
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        if isAdvancedBeauty && self.window == nil {
            TUICore.callService(TUICore_TEBeautyService, method: TUICore_TEBeautyService_CachedBeautyEffect, param: nil)
        }
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        if let advancedBeautyPanel = getAdvancedBeautyPanel() {
            beautyPanel = advancedBeautyPanel
            setBeautyMode(isAdvanced: true)
        } else {
            beautyPanel = DefaultBeautyPanel()
            setBeautyMode(isAdvanced: false)
        }
        addSubview(beautyPanel)
    }
    
    private func activateConstraints() {
        beautyPanel.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        addObserver()
        guard !isAdvancedBeauty, let panel = beautyPanel as? DefaultBeautyPanel else { return }
        panel.backClosure = { [weak self] in
            guard let self = self else { return }
            backClosure?()
        }
    }
    
    private func getAdvancedBeautyPanel() -> UIView? {
        let beautyPanelList = TUICore.getExtensionList(TUICore_TEBeautyExtension_GetBeautyPanel,
                                                       param: ["width":Screen_Width,
                                                               "height":205.scale375Height(),
                                                               "resetBeautyEffect":resetBeautyEffectForAdvancedBeauty])
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
        trtcCloud.setLocalVideoProcessDelegete(self, pixelFormat: ._32BGRA, bufferType: .pixelBuffer)
    }
    
    private func removeObserver() {
        if !isAdvancedBeauty { return }
        trtcCloud.setLocalVideoProcessDelegete(nil, pixelFormat: ._32BGRA, bufferType: .pixelBuffer)
    }
    
    deinit {
        removeObserver()
        debugPrint("\(type(of: self)) deinit")
    }
}

// MARK: Public
extension BeautyView {
    
    public static func checkIsNeedDownloadResource() -> Bool {
        if TUICore.getService(TUICore_TEBeautyService) != nil {
            guard let fileExits = TUICore.callService(TUICore_TEBeautyService,
                                                      method: TUICore_TEBeautyService_CheckResource,
                                                      param: nil,
                                                      resultCallback: nil) as? Bool else {
                return false
            }
            return !fileExits
        }
        return false
    }
}

// MARK: Action
extension BeautyView {
    @objc func backButtonClick(sender: UIButton) {
        backClosure?()
    }
}

// MARK: TRTCVideoFrameDelegate

extension BeautyView: TRTCVideoFrameDelegate {
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
