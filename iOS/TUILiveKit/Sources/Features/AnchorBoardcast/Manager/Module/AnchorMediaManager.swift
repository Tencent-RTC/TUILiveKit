//
//  AnchorMediaManager.swift
//  TUILIveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine
import TUICore

class AnchorMediaManager {
    private let observerState = ObservableState<AnchorMediaState>(initialState: AnchorMediaState())
    var mediaState: AnchorMediaState {
        observerState.state
    }
    
    private typealias Context = AnchorManager.Context
    private weak var context: Context?
    private let toastSubject: PassthroughSubject<String, Never>
    private let service: AnchorService
    private var localVideoViewObservation: NSKeyValueObservation? = nil

    init(context: AnchorManager.Context) {
        self.context = context
        self.service = context.service
        self.toastSubject = context.toastSubject
        initVideoAdvanceSettings()
    }
    
    deinit {
        enableMultiPlaybackQuality(false)
        unInitVideoAdvanceSettings()
    }
}

// MARK: - Interface
extension AnchorMediaManager {

    func prepareLiveInfoBeforeEnterRoom(liveInfo: LiveInfo) {
        enableMultiPlaybackQuality(true)
    }
    
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
    
    func onLeaveLive() {
        enableMultiPlaybackQuality(false)
        unInitVideoAdvanceSettings()
        observerState.update(isPublished: false) { state in
            state = AnchorMediaState()
        }
    }
    
    func onSelfMediaDeviceStateChanged(seatInfo: TUISeatInfo) {
        update { state in
            state.isAudioLocked = seatInfo.isAudioLocked
            state.isVideoLocked = seatInfo.isVideoLocked
        }
    }
    
    func onSelfLeaveSeat() {
        update { state in
            state.isAudioLocked = false
            state.isVideoLocked = false
        }
    }
    
    func subscribeState<Value>(_ selector: StateSelector<AnchorMediaState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Video Setting
extension AnchorMediaManager {
    func enableAdvancedVisible(_ visible: Bool) {
        observerState.update { state in
            state.videoAdvanceSettings.isVisible = visible
        }
    }
   
    func enableMultiPlaybackQuality(_ enable: Bool) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableMultiPlaybackQuality,
                            param: ["enable" : NSNumber(value: enable)])
    }
    
    private func initVideoAdvanceSettings() {
        enableMultiPlaybackQuality(true)
    }
    
    private func unInitVideoAdvanceSettings() {
        enableMultiPlaybackQuality(false)
    }
}

extension AnchorMediaManager {
    private func update(mediaState: AnchorMediaStateUpdateClosure) {
        observerState.update(reduce: mediaState)
    }
}

// MARK: - Video Advance API Extension
fileprivate extension String {
    
    static let TUICore_VideoAdvanceService = "TUICore_VideoAdvanceService"
    
    static let TUICore_VideoAdvanceService_EnableMultiPlaybackQuality = "TUICore_VideoAdvanceService_EnableMultiPlaybackQuality"
}
