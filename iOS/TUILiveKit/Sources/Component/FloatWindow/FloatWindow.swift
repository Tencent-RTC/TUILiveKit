//
//  FloatWindow.swift
//
//  Created by chensshi on 2024/11/15.
//

import SnapKit
import Foundation
import TUICore
import LiveStreamCore
import TUILiveResources
import Combine
import RTCRoomEngine

public protocol FloatWindowDataSource {
    func getRoomId() -> String
    func getOwnerId() -> String
    func getIsLinking() -> Bool
    // TODO: (gg) Need to consider the type of VoiceRoom's coreView
    func getCoreView() -> LiveCoreView
    func relayoutCoreView()
}

class FloatWindow: NSObject {
    static let shared = FloatWindow()
    private override init() {
        super.init()
        NotificationCenter.default.addObserver(self,selector: #selector(handleLogoutNotification),
                                               name: NSNotification.Name(rawValue: NSNotification.Name.TUILogoutSuccess.rawValue),
                                               object: nil)
    }
    @Published private var isShow : Bool = false
    private var floatView: FloatView?
    private var controller: (UIViewController & FloatWindowDataSource)?
    private var coreView: LiveCoreView?
    @objc private func handleLogoutNotification() {
        if isShow == true {
            releaseFloatWindow()
        }
    }

    deinit {
        NotificationCenter.default.removeObserver(self)
    }
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
        
        LiveKitLog.info("\(#file)", "\(#line)", "FloatWindow show")
        TUIRoomEngine.sharedInstance().addObserver(self)
        
        floatView.isUserInteractionEnabled = false
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak floatView] in
            floatView?.isUserInteractionEnabled = true
        }
        
        return true
    }
    
    @discardableResult
    func resumeLive(atViewController: UIViewController) -> Bool {
        guard isShow, let controller = controller else { return false }
        LiveKitLog.info("\(#file)", "\(#line)", "FloatWindow resume")
        controller.relayoutCoreView()
        controller.modalPresentationStyle = .fullScreen
        TUITool.applicationKeywindow().rootViewController?.present(controller, animated: true)
        dismiss()
        return true
    }
    
    func releaseFloatWindow() {
        LiveKitLog.info("\(#file)", "\(#line)", "FloatWindow release")
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
        guard let controller = controller else { return false }
        return controller.getIsLinking()
    }
    
    func subscribeShowingState() -> AnyPublisher<Bool, Never> {
        $isShow.eraseToAnyPublisher()
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
        TUIRoomEngine.sharedInstance().removeObserver(self)
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

// MARK: - Observer
extension FloatWindow: TUIRoomObserver {
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        releaseFloatWindow()
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        releaseFloatWindow()
    }
    
    func onKickedOffLine(message: String) {
        releaseFloatWindow()
    }
}
