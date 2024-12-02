//
//  MediaMananger.swift
//  TUILiveKit
//
//  Created by aby on 2024/11/14.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine

class VRMediaManager: VRMediaManagerInterface {
    var state: VRMediaState {
        observerState.state
    }
    
    private typealias Context = VoiceRoomManager.Context
    private let observerState = ObservableState<VRMediaState>(initialState: VRMediaState())
    private weak var context: Context?
    private let toastSubject: PassthroughSubject<String, Never>
    required init(context: VoiceRoomManager.Context) {
        self.context = context
        self.toastSubject = context.toastSubject
    }
    
    func resetState() {
        update { state in
            state.isMicrophoneOpened = false
            state.isMicrophoneMuted = false
            state.audioQuality = .default
        }
    }
}

extension VRMediaManager {
    func subscribeState<Value>(_ selector: StateSelector<VRMediaState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
    
    func subscribeState() -> AnyPublisher<VRMediaState, Never> {
        return observerState.subscribe()
    }
}

// MARK: - VRMediaManagerInterface
extension VRMediaManager {
    func update(microphoneMuted: Bool) {
        update { state in
            state.isMicrophoneMuted = microphoneMuted
        }
    }
    
    func update(microphoneOpened: Bool) {
        update { state in
            state.isMicrophoneOpened = microphoneOpened
        }
    }
}

extension VRMediaManager {
    private typealias MediaStateUpdateClosure = (inout VRMediaState) -> Void
    
    private func update(mediaState: MediaStateUpdateClosure) {
        observerState.update(reduce: mediaState)
    }
}
