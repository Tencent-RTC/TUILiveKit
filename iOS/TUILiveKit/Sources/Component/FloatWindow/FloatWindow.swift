//
//  FloatWindow.swift
//
//  Created by chensshi on 2024/11/15.
//

import SnapKit
import Foundation
import TUICore
import LiveStreamCore

protocol FloatWindowDataSource {
    func getRoomId() -> String
    func getOwnerId() -> String
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
        
        floatView.isUserInteractionEnabled = false
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak floatView] in
            floatView?.isUserInteractionEnabled = true
        }
        
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
    
    func getRoomOwnerId() -> String? {
        guard let controller = controller else { return nil }
        return controller.getOwnerId()
    }
    
    func getIsLinking() -> Bool {
        guard let coGuestState: CoGuestState = coreView?.getState() else { return false }
        return coGuestState.coGuestStatus == .linking
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
        guard let coreView = coreView else { return }
        let roomState: RoomState = coreView.getState()
        let userState: UserState = coreView.getState()
        if roomState.ownerInfo.userId == userState.selfInfo.userId {
            coreView.stopLiveStream() {} onError: { _, _ in }
        } else {
            coreView.leaveLiveStream() {} onError: { _, _ in }
        }
    }
}

// MARK: - FloatViewDelegate
extension FloatWindow: FloatViewDelegate {
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
