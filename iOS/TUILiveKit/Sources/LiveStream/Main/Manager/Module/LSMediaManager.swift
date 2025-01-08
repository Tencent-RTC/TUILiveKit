//
//  LSMediaManager.swift
//  TUILIveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine

class LSMediaManager {
    private let observerState = ObservableState<LSMediaState>(initialState: LSMediaState())
    var mediaState: LSMediaState {
        observerState.state
    }
    
    private typealias Context = LiveStreamManager.Context
    private weak var context: Context?
    private let toastSubject: PassthroughSubject<String, Never>
    private let service: LSRoomEngineService
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
        self.toastSubject = context.toastSubject
    }
    
    func resetState() {
        update { state in
            state = LSMediaState()
        }
    }
}

extension LSMediaManager {
    func subscribeState<Value>(_ selector: StateSelector<LSMediaState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Interface
extension LSMediaManager {
    func setLocalVideoView(view: UIView) {
        service.setLocalVideoView(view: view)
    }
    
    func onCameraOpened() {
        service.enableGravitySensor(enable: true)
        service.setVideoResolutionMode(.portrait)
    }
    
    func updateVideoQuality(quality: TUIVideoQuality) {
        service.updateVideoQuality(quality)
        update { state in
            state.videoQuality = quality
        }
    }
    
    func update(mediaState: LSMediaStateUpdateClosure) {
        observerState.update(reduce: mediaState)
    }
}
