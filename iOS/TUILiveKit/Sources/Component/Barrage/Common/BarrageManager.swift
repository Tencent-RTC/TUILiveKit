//
//  BarrageManager.swift
//  TUILiveKit
//
//  Created by gg on 2025/3/11.
//

import RTCRoomEngine
import Combine

class BarrageManager: NSObject {
    static let shared = BarrageManager()
    private override init() {
        super.init()
        engineManager.addObserver(self)
    }
    
    deinit {
        engineManager.removeObserver(self)
    }
    
    private var engineManager: TUIRoomEngine {
        TUIRoomEngine.sharedInstance()
    }
    
    let sendBarrageSubject = PassthroughSubject<TUIBarrage, Never>()
    let roomDismissedSubject = PassthroughSubject<String, Never>()
}

extension BarrageManager: TUIRoomObserver {
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        roomDismissedSubject.send(roomId)
    }
}
