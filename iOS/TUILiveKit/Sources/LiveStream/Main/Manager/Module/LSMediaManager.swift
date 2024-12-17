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
    
    func openLocalCamera() {
        if mediaState.isCameraOpened {
            LiveKitLog.info("\(#file)","\(#line)","camera is opened, no need to open again")
            return
        }
        Task{
            do {
                try await service.openLocalCamera(isFront: mediaState.isFrontCamera, quality: mediaState.videoQuality)
                update { state in
                    state.isCameraOpened = true
                }
                initLivingConfig()
            } catch let err {
                toastSubject.send(err.localizedDescription)
            }
        }
    }
    
    func closeLocalCamera() {
        service.closeLocalCamera()
        update { state in
            state.isCameraOpened = false
        }
    }
    
    func switchCamera() {
        let isFrontCamera = mediaState.isFrontCamera
        service.switchCamera(frontCamera: !isFrontCamera)
        update { state in
            state.isFrontCamera = !isFrontCamera
        }
    }
    
    func setCameraMirror() {
        let isMirror = mediaState.isMirror
        service.setCameraMirror(enable: !isMirror)
        update { state in
            state.isMirror = !isMirror
        }
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

// MARK: - Private
extension LSMediaManager {
    private func initLivingConfig() {
        service.enableGravitySensor(enable: true)
        service.setVideoResolutionMode(.portrait)
    }
}
