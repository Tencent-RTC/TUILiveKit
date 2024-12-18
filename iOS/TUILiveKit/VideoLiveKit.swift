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
    
    public static func createInstance() -> VideoLiveKit {
        return sharedInstance
    }
    
    public func startLive(roomId: String) {
        let viewController = TUILiveRoomAnchorViewController(roomId: roomId)
        viewController.modalPresentationStyle = .fullScreen
        getRootController()?.present(viewController, animated: true)
    }
    
    public func joinLive(liveInfo: TUILiveInfo) {
        let viewController = TUILiveRoomAudienceViewController(liveInfo: liveInfo)
        viewController.modalPresentationStyle = .fullScreen
        getRootController()?.present(viewController, animated: true)
    }
}

// MARK: - Private
extension VideoLiveKit {
    
    private func getRootController() -> UIViewController? {
        return TUITool.applicationKeywindow().rootViewController
    }
    
}
