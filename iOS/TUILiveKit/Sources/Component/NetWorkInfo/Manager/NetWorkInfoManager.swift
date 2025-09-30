//
//  NetWorkInfoManager.swift
//  Pods
//
//  Created by ssc on 2025/5/9.
//

import Foundation
import AVFoundation
import Combine
import RTCCommon
import Network
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

class NetWorkInfoManager: NSObject {
    private(set) var state: ObservableState<NetWorkInfoState>
    var netWorkInfoState: NetWorkInfoState { state.state }
    let kickedOutSubject = PassthroughSubject<Void, Never>()
    private let service: NetWorkInfoService
    private var poorNetworkTimer: Timer?
    private var poorNetworkStartTime: Date?
    private let monitor = NWPathMonitor()
    private let queue = DispatchQueue(label: "com.network.monitor")
    private var cancellables = Set<AnyCancellable>()
    private var currentPath: NWPath?
    private var hasAudioPermission: Bool = false
    private var hasVideoPermission: Bool = false
    private var isNetworkAvailable: Bool = false

    private var thermalState: ProcessInfo.ThermalState {
        ProcessInfo.processInfo.thermalState
    }
    // MARK: - Initialization
    init(service: NetWorkInfoService) {
        self.service = service
        self.state = ObservableState(initialState: NetWorkInfoState())
        super.init()

        service.addTRTCObserver(self)
        service.addRoomEngineObserver(self)
        addThermalObserver()
        initManager()
    }

    private func initManager() {
        startNetworkMonitoring()
        checkMediaPermissions()
        state.update { state in
            state.deviceTemperature = self.thermalStateDescription
            state.volume = self.service.getVolueme()
        }
    }
    
    private func startNetworkMonitoring() {
        monitor.pathUpdateHandler = { [weak self] path in
            guard let self = self else { return }
            self.currentPath = path
            
            isNetworkAvailable = path.status == .satisfied
            self.state.update { state in
                if !self.isNetworkAvailable {
                    state.netWorkQuality = .down
                }
            }
        }
        monitor.start(queue: queue)
    }

    private func checkMediaPermissions() {
        let audioStatus = AVCaptureDevice.authorizationStatus(for: .audio)
        let videoStatus = AVCaptureDevice.authorizationStatus(for: .video)

        hasAudioPermission = audioStatus == .authorized
        if hasAudioPermission == false {
            state.update { state in
                state.audioState = .close
            }
        }
        hasVideoPermission = videoStatus == .authorized
        if hasVideoPermission == false {
            state.update { state in
                state.videoState = .close
            }
        }
    }


    deinit {
        service.removeTRTCObserver(self)
        service.removeRoomEngineObserver(self)
        poorNetworkTimer?.invalidate()
        poorNetworkTimer = nil
        stopMonitoring()
        print("deinit \(type(of: self))")
    }
}

// MARK: - Minor Methods
extension NetWorkInfoManager {
    private func addThermalObserver() {
        if #available(iOS 11.0, *) {
            NotificationCenter.default
                .publisher(for: ProcessInfo.thermalStateDidChangeNotification)
                .sink { [weak self] _ in
                    let temperatureText = self?.thermalStateDescription
                    self?.state.update { state in
                        state.deviceTemperature = temperatureText ?? 0
                    }
                }
                .store(in: &cancellables)
        } else {
            return
        }
    }

    private var thermalStateDescription: Int {
        switch thermalState {
            case .nominal: return 0
            case .fair: return 1
            case .serious: return 2
            case .critical: return 3
            @unknown default: return -1
        }
    }

    private func stopMonitoring() {
        monitor.cancel()
        cancellables.forEach { $0.cancel() }
    }
}

// MARK: - onChanged Methods
extension NetWorkInfoManager {
    func handleAudioSliderValueChanged(volume: Int) {
        service.setAudioCaptureVolume(volume: volume)
        state.update { state in
            state.volume = volume
        }
    }

    func onNetWorkInfoStatusToastViewClosed() {
        poorNetworkTimer?.invalidate()
        poorNetworkTimer = nil
        state.update { state in
            state.showToast = false
        }
    }

    func onAudioQualityChanged(_ quality: TUIAudioQuality) {
        service.updateAudioQuality(quality: quality)
        state.update { state in
            state.audioQuality = quality
        }
    }
}


// MARK: - State Subscription Extension
extension NetWorkInfoManager {
    func subscribe<Value>(_ selector: StateSelector<NetWorkInfoState, Value>) -> AnyPublisher<Value, Never> {
        return state.subscribe(selector)
    }
}

// MARK: - TRTC Callback Extension
extension NetWorkInfoManager: TRTCCloudDelegate {
    func onStatistics(_ statistics: TRTCStatistics) {

        guard let bigStreamStats = statistics.localStatistics.first(where: { $0.streamType == .big }) else {
            return
        }
        
        state.update { state in
            let videoWidth = Int(bigStreamStats.width)
            state.videoResolution = videoWidth

            if !hasVideoPermission {
                return
            } else if state.videoState == .close {
                state.videoState = .close
            } else {
                let isLowFrameRate = bigStreamStats.frameRate < 15
                let isLowBitrate: Bool = {
                    switch videoWidth {
                        case 240:  return bigStreamStats.videoBitrate < 100
                        case 360:  return bigStreamStats.videoBitrate < 200
                        case 480:  return bigStreamStats.videoBitrate < 350
                        case 540:
                            return bigStreamStats.videoBitrate < (bigStreamStats.height == 960 ? 800 : 500)
                        case 1080: return bigStreamStats.videoBitrate < 1500
                        default:   return false
                    }
                }()

                state.videoState = (isLowFrameRate || isPoorNetworkQuality(state.netWorkQuality) || isLowBitrate)
                ? .exception
                : .normal
            }
            
            if !hasAudioPermission {
                return
            } else if state.audioState == .close {
                state.audioState = .close
            } else if state.volume == 0 {
                state.audioState = .mute
            } else if bigStreamStats.audioCaptureState != 0  {
                state.audioState = .exception
            } else {
                state.audioState = .normal
            }
        }
    }
}

// MARK: - RoomEngine Callback Extension
extension NetWorkInfoManager: TUIRoomObserver {
    func onUserNetworkQualityChanged(networkList: [TUINetworkInfo]) {
        if let currentPath = currentPath, currentPath.status != .satisfied {
            return
        }
        
        let userId = service.getSelfUserId()
        if let matchedInfo = networkList.first(where: { $0.userId == userId }) {
            state.update { state in
                state.rtt = matchedInfo.delay
                state.downLoss = matchedInfo.downLoss
                state.upLoss = matchedInfo.upLoss
                if isNetworkAvailable {
                    state.netWorkQuality = matchedInfo.quality
                }
            }
            handleNetworkQualityChange(matchedInfo.quality)
        }
    }

    func onUserAudioStateChanged(userId:String ,hasAudio: Bool, reason: TUIChangeReason) {
        if userId == service.getSelfUserId() {
            state.update{ state in
                if hasAudio {
                    state.audioState = .normal
                } else {
                    state.audioState = .close
                }
            }
        }
    }

    func onUserVideoStateChanged(userId:String ,streamType: TUIVideoStreamType,hasVideo : Bool,reason: TUIChangeReason) {
        if userId == service.getSelfUserId() {
            state.update{ state in
                if hasVideo {
                    state.videoState = .normal
                } else {
                    state.videoState = .close
                }
            }
        }
    }
    
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        kickedOutSubject.send()
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        kickedOutSubject.send()
    }
}

// MARK: - Private Extension
extension NetWorkInfoManager {
    private func handleNetworkQualityChange(_ quality: TUINetworkQuality) {
        if netWorkInfoState.showToast == true {
            return
        }
        if isPoorNetworkQuality(quality) {
            startPoorNetworkCountdown()
        } else {
            stopPoorNetworkCountdown()
        }
    }

    private func isPoorNetworkQuality(_ quality: TUINetworkQuality) -> Bool {
        return quality == .veryBad || quality == .bad || quality == .down
    }

    private func startPoorNetworkCountdown() {
        guard poorNetworkTimer == nil else {
            return }
        var countdown = 30
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.poorNetworkTimer?.invalidate()
            self.poorNetworkTimer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { [weak self] timer in
                countdown -= 1
                if countdown <= 0 {
                    self?.state.update { state in
                        state.showToast = true
                    }
                    timer.invalidate()
                    self?.poorNetworkTimer = nil
                    
                    DispatchQueue.main.asyncAfter(deadline: .now() + 5) {
                        self?.state.update { state in
                            state.showToast = false
                        }
                    }
                }
            }
        }
    }

    private func stopPoorNetworkCountdown() {
        poorNetworkTimer?.invalidate()
        poorNetworkTimer = nil
        state.update { state in
            state.showToast = false
        }
    }
}
