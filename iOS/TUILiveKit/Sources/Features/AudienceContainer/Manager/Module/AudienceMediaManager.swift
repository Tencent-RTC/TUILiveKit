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

class AudienceMediaManager: NSObject {
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
        super.init()
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
    
    func onJoinLive(liveInfo: TUILiveInfo) {
        getMultiPlaybackQuality(roomId: liveInfo.roomId)
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
    
    private func initVideoAdvanceSettings() {
        enableSwitchPlaybackQuality(true)
    }
    
    private func unInitVideoAdvanceSettings() {
        enableSwitchPlaybackQuality(false)
    }
}

// MARK: - Multi Playback Quality
extension AudienceMediaManager {
    
    func switchPlaybackQuality(quality: TUIVideoQuality) {
        service.switchPlaybackQuality(quality)
    }
    
    func getMultiPlaybackQuality(roomId: String) {
        service.getMultiPlaybackQuality(roomId: roomId) { [weak self] qualityList in
            guard let self = self else { return }
            self.observerState.update { mediaState in
                mediaState.playbackQualityList = qualityList
                mediaState.playbackQuality = qualityList.first
            }
        }
    }
    
    func enableSwitchPlaybackQuality(_ enable: Bool) {
        TUICore.callService(.TUICore_VideoAdvanceService,
                            method: .TUICore_VideoAdvanceService_EnableSwitchMultiPlayback,
                            param: ["enable" : NSNumber(value: enable)])
    }
    
    func onUserVideoSizeChanged(roomId: String,
                                userId: String,
                                streamType: TUIVideoStreamType,
                                width: Int32,
                                height: Int32) {
        guard let context = context else {
            return
        }
        let playbackQuality = getVideoQuality(width: width, height: height)
        guard playbackQuality != mediaState.playbackQuality else {
            return
        }
        guard mediaState.playbackQualityList.count > 1, mediaState.playbackQualityList.contains(playbackQuality) else {
            return
        }
        guard context.coGuestManager.coGuestState.coGuestStatus == .none else {
            return
        }
        observerState.update { mediaState in
            mediaState.playbackQuality = playbackQuality
        }
    }
    
    private func getVideoQuality(width: Int32, height: Int32) -> TUIVideoQuality {
        if (width * height) <= (360 * 640) {
            return TUIVideoQuality.quality360P
        }
        if (width * height) <= (540 * 960){
            return TUIVideoQuality.quality540P
        }
        if (width * height) <= (720 * 1280) {
            return TUIVideoQuality.quality720P
        }
        return TUIVideoQuality.quality1080P
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
    
    static let TUICore_VideoAdvanceService_EnableSwitchMultiPlayback = "TUICore_VideoAdvanceService_EnableSwitchMultiPlayback"
}
