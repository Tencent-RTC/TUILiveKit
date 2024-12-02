//
//  StreamDashboardState.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/21.
//

import Foundation
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

struct StreamDashboardState: Encodable {

    var roomId: String = ""
    
    var rtt: UInt32 = 0
    var downLoss: UInt32 = 0
    var upLoss: UInt32 = 0
    
    var localUsers: [StreamDashboardUser] = []
    var remoteUsers: [StreamDashboardUser] = []
}


struct StreamDashboardUser: Encodable {
    
    var userId: String = ""
    var isLocal: Bool {
        return userId == ""
    }
    
    var videoResolution: String = ""
    var videoFrameRate: UInt32 = 0
    var videoBitrate: UInt32 = 0
    
    var streamType: TRTCVideoStreamType = .big
    
    var audioSampleRate: UInt32 = 0
    var audioBitrate: UInt32 = 0
    
    init() {
        
    }
    
    init(local: TRTCLocalStatistics) {
        self.videoResolution = "\(local.width)x\(local.height)"
        self.videoFrameRate = local.frameRate
        self.videoBitrate = local.videoBitrate
        self.streamType = local.streamType
        
        self.audioBitrate = local.audioBitrate
        self.audioSampleRate = local.audioSampleRate
    }
    
    init(remote: TRTCRemoteStatistics) {
        self.userId = remote.userId
        
        self.videoResolution = "\(remote.width)x\(remote.height)"
        self.videoFrameRate = remote.frameRate
        self.videoBitrate = remote.videoBitrate
        self.streamType = remote.streamType
        
        self.audioBitrate = remote.audioBitrate
        self.audioSampleRate = remote.audioSampleRate
    }
}

extension StreamDashboardUser: Equatable {}
extension StreamDashboardUser: Hashable {
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(isLocal ? "local":userId)
    }
    
}


extension TRTCVideoStreamType: Codable {
    public init(from decoder: Decoder) throws {
        let value = try decoder.singleValueContainer().decode(Int.self)
        switch value {
        case 0:
            self = .big
        case 1:
            self = .small
        case 2:
            self = .sub
        default:
            self = .big
        }
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        switch self {
        case .big:
            try container.encode(0)
        case .small:
            try container.encode(1)
        case .sub:
            try container.encode(2)
        default:
            try container.encode(0)
        }
    }
}
