//
//  BeautyManager.swift
//  AFNetworking
//
//  Created by gg on 2024/12/11.
//

import Foundation
import RTCRoomEngine
import RTCCommon

protocol BeautyManagerInterface {
    func setSmoothLevel(_ level: Float)
    func setWhitenessLevel(_ level: Float)
    func setRuddyLevel(_ level: Float)
    
    func setLocalVideoView(_ view: UIView)
    func openLocalCamera()
}

class BeautyManager {
    
    private let service = BeautyService(roomEngine: TUIRoomEngine.sharedInstance())
    private let stateKey = "__kBeautyManager_state_key__"
    
    func isCloseBeauty() -> Bool {
        return (state.smoothLevel == 0) && (state.whitenessLevel == 0) && (state.ruddyLevel == 0)
    }
    
    func closeBeauty() {
        service.closeBeauty()
    }
    
    init() {
        StateCache.shared.subscribeToObjectRemoval(key: stateKey) {
            BeautyService.closeBeauty()
        }
    }
}

extension BeautyManager: BeautyManagerInterface {
    func setSmoothLevel(_ level: Float) {
        service.setBeautyLevel(level)
        update { state in
            state.smoothLevel = level
        }
    }
    
    func setWhitenessLevel(_ level: Float) {
        service.setWhitenessLevel(level)
        update { state in
            state.whitenessLevel = level
        }
    }
    
    func setRuddyLevel(_ level: Float) {
        service.setRuddyLevel(level)
        update { state in
            state.ruddyLevel = level
        }
    }
    
    func setLocalVideoView(_ view: UIView) {
        service.setLocalVideoView(view)
    }
    
    func openLocalCamera() {
        Task {
            do {
                try await service.openLocalCamera()
            } catch let err as InternalError {
                debugPrint("openLocalCamera error: \(err.localizedMessage)")
            }
        }
    }
}

extension BeautyManager {
    private typealias BeautyStateUpdateClosure = (inout BeautyState) -> Void
    private var observerState: ObservableState<BeautyState> {
        if let state: ObservableState<BeautyState> = StateCache.shared[stateKey] {
            return state
        }
        let newState = ObservableState<BeautyState>(initialState: BeautyState())
        StateCache.shared.setObject(key: stateKey, obj: newState)
        if let state: ObservableState<BeautyState> = StateCache.shared[stateKey] {
            return state
        }
        return newState
    }
    
    private func update(state: BeautyStateUpdateClosure) {
        observerState.update(reduce: state)
    }
    
    var state: BeautyState {
        observerState.state
    }
}
