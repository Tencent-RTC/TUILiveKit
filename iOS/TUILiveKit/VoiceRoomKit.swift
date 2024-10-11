//
//  VoiceRoomKit.swift
//  TUILiveKit
//
//  Created by jack on 2024/10/8.
//

import Foundation
import TUICore

@objcMembers
public class VoiceRoomKit: NSObject {
    
    private static let sharedInstance = VoiceRoomKit()
    
    private override init() {}
    
    public static func createInstance() -> VoiceRoomKit {
        return sharedInstance
    }
    
    public func createRoom(roomId: String, params: CreateRoomParams) {
        let roomParams = RoomParams()
        roomParams.maxSeatCount = params.maxAnchorCount
        roomParams.seatMode = params.seatMode
        let viewController = TUIVoiceRoomViewController(roomId: roomId, behavior: .prepareCreate, roomParams: roomParams)
        viewController.modalPresentationStyle = .fullScreen
        getRootController()?.present(viewController, animated: true)
    }
    
    public func enterRoom(roomId: String) {
        let viewController = TUIVoiceRoomViewController(roomId: roomId, behavior: .join)
        viewController.modalPresentationStyle = .fullScreen
        getRootController()?.present(viewController, animated: true)
    }
}


// MARK: - Private
extension VoiceRoomKit {
    
    private func getRootController() -> UIViewController? {
        return TUITool.applicationKeywindow().rootViewController
    }
    
}
