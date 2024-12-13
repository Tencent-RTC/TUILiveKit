//
//  IMObserver.swift
//  TUILiveKit
//
//  Created by gg on 2024/12/9.
//

import Foundation
import ImSDK_Plus

class IMObserver: NSObject {
    private(set) weak var context: LiveStreamManager.Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        super.init()
    }
}

extension IMObserver: V2TIMSDKListener {
    func onConnectSuccess() {
        // TODO: Need fix by RoomEngine
        context?.roomManager.onReconnected()
    }
}
