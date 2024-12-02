//
//  FloatWindow.swift
//
//  Created by chensshi on 2024/11/15.
//

import SnapKit
import Foundation
import TUICore
//import RTCCommon
import LiveStreamCore

protocol FloatWindowDataSource {
    func getRoomId() -> String
    // TODO: (gg) Need to consider the type of VoiceRoom's coreView
    func getCoreView() -> LiveCoreView
    func relayoutCoreView()
}

class FloatWindow {
    static let shared = FloatWindow()
    private init() {}
    private var isShow : Bool = false
    private var floatView: FloatView?
    private var controller: (UIViewController & FloatWindowDataSource)?
    private var coreView: LiveCoreView?
}

// MARK: -------------- API --------------
extension FloatWindow {
    @discardableResult
    func showFloatWindow(controller: UIViewController & FloatWindowDataSource) -> Bool {
        guard !isShow else { return false }
        guard let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene,
              let window = windowScene.windows.first(where: { $0.isKeyWindow }) else { return false }
        
        self.controller = controller
        let coreView = controller.getCoreView()
        self.coreView = coreView
        
        if let nav = controller.navigationController {
            nav.popViewController(animated: true)
        } else {
            controller.dismiss(animated: true)
        }
        
        coreView.removeFromSuperview()
        
        let floatView = FloatView(contentView: coreView)
        floatView.layoutSubviews()
        floatView.delegate = self
        window.addSubview(floatView)
        self.floatView = floatView
        
        isShow = true
        return true
    }
    
    @discardableResult
    func resumeLive(atViewController: UIViewController) -> Bool {
        guard isShow, let controller = controller else { return false }
        controller.relayoutCoreView()
        if let nav = controller.navigationController, nav == atViewController {
            nav.pushViewController(controller, animated: true)
        } else if let nav = atViewController as? UINavigationController {
            nav.pushViewController(controller, animated: true)
        } else {
            atViewController.present(controller, animated: true)
        }
        dismiss()
        return true
    }
    
    func releaseFloatWindow() {
        leaveRoom()
        dismiss()
    }
    
    func isShowingFloatWindow() -> Bool {
        return isShow
    }
    
    func getCurrentRoomId() -> String? {
        guard let controller = controller else { return nil }
        return controller.getRoomId()
    }
}

// MARK: -------------- IMPL --------------
private extension FloatWindow {
    func dismiss() {
        controller = nil
        coreView = nil
        floatView?.removeFromSuperview()
        floatView = nil
        isShow = false
    }
    
    func leaveRoom() {
        coreView?.leaveLiveStream() {
        } onError: { _, _ in
        }
    }
}

// MARK: - FloatViewDelegate
extension FloatWindow: FloatViewDelegate {
    func onClose() {
        releaseFloatWindow()
    }
    
    func onResume() {
        if let nav = controller?.navigationController {
            resumeLive(atViewController: nav)
        } else if let vc = TUITool.applicationKeywindow().rootViewController {
            resumeLive(atViewController: vc)
        } else {
            LiveKitLog.info("\(#file)", "\(#line)","FloatWindow onResume cant found controller to present")
            releaseFloatWindow()
        }
    }
}
