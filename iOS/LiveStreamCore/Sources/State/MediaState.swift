//
//  MediaState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

public struct MediaState {
    public var hasMicrophonePermission: Bool = false
    public var isMicrophoneOpened: Bool = false
    public var isMicrophoneMuted: Bool = true
    public var isCameraOpened: Bool = false
    public var isFrontCamera: Bool = true

    public var videoEncParams: VideoEncParams = VideoEncParams()
    
    public var isMirrorEnabled: Bool = false
    public var videoAdvanceSettings: VideoAdvanceSetting = VideoAdvanceSetting()
}

public struct VideoEncParams {
    
    public enum VideoEncType {
        case big, small
    }
    
    public var currentEncType: VideoEncParams.VideoEncType = .big
    
    public var currentEnc: TUIRoomVideoEncoderParams {
        set {
            if currentEncType == .small {
                small = newValue
            }
            big = newValue
        }
        get {
            if currentEncType == .small {
                return small
            }
            return big
        }
    }
    
    public var big: TUIRoomVideoEncoderParams = {
        let param = TUIRoomVideoEncoderParams()
        param.videoResolution = .quality1080P
        param.bitrate = 6000
        param.fps = 30
        param.resolutionMode = .portrait
        return param
    }()
    public var small: TUIRoomVideoEncoderParams = {
        let param = TUIRoomVideoEncoderParams()
        param.videoResolution = .quality540P
        param.bitrate = 1500
        param.fps = 30
        param.resolutionMode = .portrait
        return param
    }()
}

public struct VideoAdvanceSetting {
    
    public var isVisible: Bool = false
    
    public var isUltimateEnabled: Bool = false
    
    public var isBFrameEnabled: Bool = false
    
    public var isH265Enabled: Bool = false
    
    public var hdrRenderType: HDRRenderType = .none
}

public enum HDRRenderType: Int {
    case none = 0
    case displayLayer = 1
    case metal = 2
}
