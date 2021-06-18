//
//  LiveRoomToastManager.swift
//  TUILiveRoom
//
//  Created by adams on 2021/5/26.
//

import UIKit
import Toast_Swift

@objcMembers
public class LiveRoomToastManager: NSObject {
    private static let staticInstance: LiveRoomToastManager = LiveRoomToastManager.init()
    public static func sharedManager() -> LiveRoomToastManager { staticInstance }
    private override init(){}
    
    public func setupToast() {
        ToastManager.shared.position = .center
    }
    
    public func makeToast(view: UIView, message: String) {
        view.makeToast(message)
    }
    
    public func makeToast(view: UIView, message: String, duration: TimeInterval) {
        view.makeToast(message, duration: duration)
    }
}
