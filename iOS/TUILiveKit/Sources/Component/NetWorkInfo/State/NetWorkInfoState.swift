//
//  NetWorkInfo.swift
//  Pods
//
//  Created by ssc on 2025/5/9.
//
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif


struct NetWorkInfoState {
    var videoState: VideoState = .normal
    var videoResolution: Int = 720

    var audioState: AudioState = .normal
    var audioQuality: TUIAudioQuality = .default
    var volume: Int = 50

    var rtt: UInt32 = 0
    var upLoss: UInt32 = 0
    var downLoss: UInt32 = 0
    var uploadSpeed: UInt64 = 0
    var netWorkQuality: TUINetworkQuality = .excellent

    var deviceTemperature: Int = 0
    var showToast: Bool = false
}
enum AudioState {
    case mute
    case close
    case normal
    case exception
}

enum VideoState {
    case close
    case normal
    case exception
}
