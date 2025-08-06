//
//  VoiceRoomKit.swift
//  TUILiveKit
//
//  Created by jack on 2024/10/8.
//

import Foundation
import TUICore
import SwiftUI

@objcMembers
public class KTVRoomKit: NSObject {
    
    private static let sharedInstance = KTVRoomKit()
    
    private override init() {}
    
    public static func createInstance() -> KTVRoomKit {
        return sharedInstance
    }
    
    public func createRoom(roomId: String) {        
        let host = PortraitHostingController(rootView: KTVRootView(page: .PreparePage(liveId: roomId)))
        host.modalPresentationStyle = .fullScreen
        getRootController()?.present(host, animated: true)
    }
    
    public func enterRoom(roomId: String) {
        let host = PortraitHostingController(rootView: KTVRootView(page: .guestPage(liveId: roomId)))
        host.modalPresentationStyle = .fullScreen
        getRootController()?.present(host, animated: true)
    }
}


// MARK: - Private
extension KTVRoomKit {
    
    private func getRootController() -> UIViewController? {
        return TUITool.applicationKeywindow().rootViewController
    }
    
}


class PortraitHostingController<Content: View>: UIHostingController<Content> {
    override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        return .portrait
    }
    
    override var preferredInterfaceOrientationForPresentation: UIInterfaceOrientation {
        return .portrait
    }
    
    override var shouldAutorotate: Bool {
        return false
    }
}
