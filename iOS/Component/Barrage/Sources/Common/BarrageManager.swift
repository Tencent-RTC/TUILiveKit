//
//  BarrageManager.swift
//  TUILiveKit
//
//  Created by gg on 2025/3/11.
//

import RTCRoomEngine
import Combine
import RTCCommon

class BarrageManager: NSObject {
    static let shared = BarrageManager()
    
    private static let stateKey = "__kBarrageManager_state_key__"
    private override init() {
        super.init()
        engineManager.addObserver(self)
        subscribe()
    }
    
    private func subscribe() {
        StateCache.shared.subscribeToObjectRemoval(key: BarrageManager.stateKey) {
            BarrageManager.shared.inputString = ""
            DispatchQueue.main.async {
                BarrageManager.shared.subscribe()
            }
        }
    }
    
    deinit {
        engineManager.removeObserver(self)
    }
    
    private var engineManager: TUIRoomEngine {
        TUIRoomEngine.sharedInstance()
    }
    
    var inputString: String = ""
    let toastSubject = PassthroughSubject<String, Never>()
    let sendBarrageSubject = PassthroughSubject<TUIBarrage, Never>()
    let roomDismissedSubject = PassthroughSubject<String, Never>()
}

extension BarrageManager: TUIRoomObserver {
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        roomDismissedSubject.send(roomId)
    }
}
