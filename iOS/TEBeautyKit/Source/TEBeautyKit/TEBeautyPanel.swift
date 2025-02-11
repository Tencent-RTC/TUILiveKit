//
//  TEBeautyPanel.swift
//  TEBeautyKit
//
//  Created by jack on 2024/12/31.
//  Copyright (c) 2024 Tencent.

import Foundation
import CoreVideo
import TencentEffect
import TUICore
import SnapKit

class TEBeautyPanel: UIView {
    
    private var xmagic: XMagic?
    private var beautyKit: TEBeautyKit?
    private var panelView: TEPanelView?
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        self.frame = frame
        initBeautyKit(frame: frame)
    }
    
    deinit {
        unInitBeautyKit()
    }
    
    private func initBeautyKit(frame: CGRect) {
        initBeautyJson()
        beautyKit = TEBeautyKit()
        panelView = TEPanelView(nil, comboType: nil)
        panelView?.teBeautyKit = beautyKit
        addSubview(panelView!)
        panelView?.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
            make.size.equalTo(frame.size)
        })
        initXMagic()
    }
    
    private func unInitBeautyKit() {
        if let xmagic = self.xmagic {
            xmagic.onPause()
            xmagic.clearListeners()
            xmagic.deinit()
            self.xmagic = nil
        }
        beautyKit = nil
        panelView?.removeFromSuperview()
    }
    
    private func initBeautyJson() {
        guard let resourcePath = Bundle.main.path(forResource: "TencentEffectResources", ofType: "bundle") else {
            return
        }
        guard let bundle = Bundle(path: resourcePath) else {
            return
        }
        TEUIConfig.shareInstance().setTEPanelViewRes(bundle.path(forResource: "beauty", ofType: "json") ?? "",
                                                     beautyBody: bundle.path(forResource: "beauty_body", ofType: "json") ?? "",
                                                     lut: bundle.path(forResource: "lut", ofType: "json") ?? "",
                                                     motion: Bundle.beautyKitBundle.path(forResource: "motion", ofType: "json") ?? "",
                                                     makeup: bundle.path(forResource: "makeup", ofType: "json") ?? "",
                                                     segmentation: bundle.path(forResource: "segmentation", ofType: "json") ?? "")
    }
    
    private func initXMagic() {
        TEBeautyKit.create { [weak self] xMagic in
            guard let self = self else { return }
            guard let xmagic = xMagic else { return }
            self.xmagic = xmagic
            let bundleDic = [
                "BODY3D_POINT_AGENT": "\(TCDownloadManager.share().getResPath())/LightBodyPlugin/models/LightBody3DModel.bundle",
                "BODY_AGENT": "\(TCDownloadManager.share().getResPath())/LightBodyPlugin/models/LightBodyModel.bundle",
            ]
            xmagic.setBundleToLightEngine(bundleDic)
            self.beautyKit?.setXMagicApi(xmagic)
            self.beautyKit?.setLogLevel(.YT_SDK_ERROR_LEVEL)
            self.beautyKit?.setTePanelView(self.panelView)
            
            self.panelView?.teBeautyKit = self.beautyKit
            self.panelView?.beautyKitApi = xmagic
        }
    }
}

// MARK: - Public
extension TEBeautyPanel {
    
    public static func checkResource(completion: (() -> ())?) -> Bool {
        if CommonUtil.fileIsExists() {
            completion?()
            return true
        }
        let controller = TransparentPresentationController()
        controller.titleName = .localize("Beauty")
        controller.downloadAvatarRes {
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                completion?()
            }
        }
        controller.hidesBottomBarWhenPushed = true
        controller.modalPresentationStyle = .fullScreen
        getCurrentWindowViewController()?.present(controller, animated: false)
        return false
    }
    
    public func processVideoFrame(textureId: Int32, textureWidth: Int32, textureHeight: Int32) -> Int32 {
        guard let beautyKit = beautyKit else {
            return textureId
        }
        let output = beautyKit.processTexture(textureId,
                                              textureWidth: textureWidth,
                                              textureHeight: textureHeight,
                                              with: .topLeft,
                                              with: .cameraRotation0)
        return output?.textureData?.texture ?? 0
    }
    
    public func processVideoFrame(pixelBuffer: CVPixelBuffer) -> CVPixelBuffer {
        guard let beautyKit = beautyKit else {
            return pixelBuffer
        }
        let output = beautyKit.processPixelData(pixelBuffer,
                                                pixelDataWidth: Int32(CVPixelBufferGetWidth(pixelBuffer)),
                                                pixelDataHeight: Int32(CVPixelBufferGetHeight(pixelBuffer)),
                                                with: .topLeft,
                                                with: .cameraRotation0)
        return output?.pixelData?.data ?? pixelBuffer
    }
}

// MARK: - Private
extension TEBeautyPanel {
    
    private static func getCurrentWindowViewController() -> UIViewController? {
        var keyWindow: UIWindow?
        for window in UIApplication.shared.windows {
            if window.isMember(of: UIWindow.self), window.isKeyWindow {
                keyWindow = window
                break
            }
        }
        guard let rootController = keyWindow?.rootViewController else {
            return nil
        }
        func findCurrentController(from vc: UIViewController?) -> UIViewController? {
            if let nav = vc as? UINavigationController {
                return findCurrentController(from: nav.topViewController)
            } else if let tabBar = vc as? UITabBarController {
                return findCurrentController(from: tabBar.selectedViewController)
            } else if let presented = vc?.presentedViewController {
                return findCurrentController(from: presented)
            }
            return vc
        }
        let viewController = findCurrentController(from: rootController)
        return viewController
    }
    
}
