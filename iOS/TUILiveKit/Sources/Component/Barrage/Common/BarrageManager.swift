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
    
    var inputString: String = ""
    let toastSubject = PassthroughSubject<String, Never>()
}
