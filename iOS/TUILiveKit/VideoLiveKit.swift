//
//  VideoLiveKit.swift
//  TUILiveKit-TUILiveKitBundle
//
//  Created by jack on 2024/9/29.
//

import Foundation
import TUICore
import RTCRoomEngine

@objcMembers
public class VideoLiveKit: NSObject {
    
    private static let sharedInstance = VideoLiveKit()
    
    private override init() {}
    
    private weak var viewController: UIViewController?
    
    public static func createInstance() -> VideoLiveKit {
        return sharedInstance
    }
    
    @MainActor
    public func enableFollowFeature(_ enable: Bool) {
        enableFollow = enable
    }
    
    @MainActor
    public func startLive(roomId: String) {
        
        if FloatWindow.shared.isShowingFloatWindow() {
            if let ownerId = FloatWindow.shared.getRoomOwnerId(), ownerId == TUILogin.getUserID() {
                getRootController()?.view.makeToast(.pushingToReturnText)
                return
            } else if FloatWindow.shared.getIsLinking() {
                getRootController()?.view.makeToast(.pushingToReturnText)
                return
            }
        }
        
        let viewController = TUILiveRoomAnchorViewController(roomId: roomId)
        viewController.modalPresentationStyle = .fullScreen
        
        getRootController()?.present(viewController, animated: true)
        self.viewController = viewController
    }
    
    @MainActor
    public func stopLive(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        guard let vc = viewController as? TUILiveRoomAnchorViewController else {
            onSuccess?()
            return
        }

        vc.stopLive { [weak self] in
            guard let self = self else { return }
            onSuccess?()
            self.viewController = nil
        } onError: { code, message in
            onError?(code, message)
        }
    }
    
    @MainActor
    public func joinLive(roomId: String) {
        
        let viewController = TUILiveRoomAudienceViewController(roomId: roomId)
        viewController.modalPresentationStyle = .fullScreen
        
        getRootController()?.present(viewController, animated: true)
        self.viewController = viewController
    }
    
    @MainActor
    public func leaveLive(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        if let vc = viewController as? TUILiveRoomAudienceViewController {
            vc.leaveLive { [weak self] in
                guard let self = self else { return }
                onSuccess?()
                self.viewController = nil
            } onError: { code, message in
                onError?(code, message)
            }
        } else if FloatWindow.shared.isShowingFloatWindow() {
            guard FloatWindow.shared.getRoomOwnerId() != TUILogin.getUserID() else { return }
            FloatWindow.shared.releaseFloatWindow()
        } else {
            onSuccess?()
        }
    }
    
    var enableFollow: Bool = true
}

// MARK: - Private
extension VideoLiveKit {
    
    private func getRootController() -> UIViewController? {
        return TUITool.applicationKeywindow().rootViewController
    }
}

extension String {
    fileprivate static let pushingToReturnText = localized("live.error.pushing")
}
