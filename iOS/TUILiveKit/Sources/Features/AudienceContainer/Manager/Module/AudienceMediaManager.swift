//
//  AudienceMediaManager.swift
//  TUILIveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine
import TUICore

class AudienceMediaManager {
    private let observerState = ObservableState<AudienceMediaState>(initialState: AudienceMediaState())
    var mediaState: AudienceMediaState {
        observerState.state
    }
    
    private typealias Context = AudienceManager.Context
    private weak var context: Context?
    private let toastSubject: PassthroughSubject<String, Never>
    private let service: AudienceService
    private var localVideoViewObservation: NSKeyValueObservation? = nil

    init(context: AudienceManager.Context) {
        self.context = context
        self.service = context.service
        self.toastSubject = context.toastSubject
        initVideoAdvanceSettings()
    }
    
    deinit {
        unInitVideoAdvanceSettings()
    }
}

// MARK: - Interface
extension AudienceMediaManager {
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
        unInitVideoAdvanceSettings()
        observerState.update(isPublished: false) { state in
            state = AudienceMediaState()
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
    
    func subscribeState<Value>(_ selector: StateSelector<AudienceMediaState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Video Setting
extension AudienceMediaManager {
    func enableAdvancedVisible(_ visible: Bool) {
        observerState.update { state in
            state.videoAdvanceSettings.isVisible = visible
        }
    }
    
    func enableUltimate(_ enable: Bool) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableUltimate,
                            param: ["enable" : NSNumber(value: enable)])
        observerState.update { state in
            state.videoAdvanceSettings.isUltimateEnabled = enable
        }
        if enable {
            enableBFrame(false)
        }
    }
    
    func enableBFrame(_ enable: Bool) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableBFrame,
                            param: ["enable" : NSNumber(value: enable)])
        observerState.update { state in
            state.videoAdvanceSettings.isBFrameEnabled = enable
        }
    }
    
    func enableH265(_ enable: Bool) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableH265,
                            param: ["enable" : NSNumber(value: enable)])
        observerState.update { state in
            state.videoAdvanceSettings.isH265Enabled = enable
        }
    }
    
    func enableHDR(_ renderType: AudienceHDRRenderType) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableHDR,
                            param: ["renderType" : NSNumber(value: renderType.rawValue)])
        observerState.update { state in
            state.videoAdvanceSettings.hdrRenderType = renderType
        }
    }
    
    private func initVideoAdvanceSettings() {
        enableUltimate(true)
        enableH265(true)
    }
    
    private func unInitVideoAdvanceSettings() {
        enableUltimate(false)
        enableH265(false)
    }
}

extension AudienceMediaManager {
    private func update(mediaState: AudienceMediaStateUpdateClosure) {
        observerState.update(reduce: mediaState)
    }
}

// MARK: - Video Advance API Extension
fileprivate extension String {
    
    static let TUICore_VideoAdvanceService = "TUICore_VideoAdvanceService"
    
    static let TUICore_VideoAdvanceService_EnableUltimate = "TUICore_VideoAdvanceService_EnableUltimate"
    static let TUICore_VideoAdvanceService_EnableH265 = "TUICore_VideoAdvanceService_EnableH265"
    static let TUICore_VideoAdvanceService_EnableHDR = "TUICore_VideoAdvanceService_EnableHDR"
    static let TUICore_VideoAdvanceService_EnableBFrame = "TUICore_VideoAdvanceService_EnableBFrame"
}
