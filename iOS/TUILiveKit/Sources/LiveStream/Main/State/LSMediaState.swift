//
//  LSMediaState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import RTCRoomEngine

struct LSMediaState {
    var audioQuality: TUIAudioQuality = .default
    var videoQuality: TUIVideoQuality = .quality1080P
    var isAudioLocked: Bool = false
    var isVideoLocked: Bool = false
    
    var videoEncParams: VideoEncParams = VideoEncParams.shared
    var videoAdvanceSettings: VideoAdvanceSetting = VideoAdvanceSetting()
}

class VideoEncParams {
    
    static let shared = VideoEncParams()
    private init() {}
    
    enum VideoEncType {
        case big, small
    }
    
    var currentEncType: VideoEncParams.VideoEncType = .big
    
    var currentEnc: TUIRoomVideoEncoderParams {
        set {
            if currentEncType == .small {
                small = newValue
            } else {
                big = newValue
            }
        }
        get {
            if currentEncType == .small {
                return small
            }
            return big
        }
    }
    
    var big: TUIRoomVideoEncoderParams = {
        let param = TUIRoomVideoEncoderParams()
        param.videoResolution = .quality1080P
        param.bitrate = 4000
        param.fps = 30
        param.resolutionMode = .portrait
        return param
    }()
    var small: TUIRoomVideoEncoderParams = {
        let param = TUIRoomVideoEncoderParams()
        param.videoResolution = .quality540P
        param.bitrate = 1800
        param.fps = 30
        param.resolutionMode = .portrait
        return param
    }()
    
    func reset() {
        big = getDefaultParam(encType: .big)
        small = getDefaultParam(encType: .small)
        currentEncType = .big
        currentEnc = big
    }
    
    func getDefaultParam(encType: VideoEncType) -> TUIRoomVideoEncoderParams {
        let param = TUIRoomVideoEncoderParams()
        if encType == .big {
            param.videoResolution = .quality1080P
            param.bitrate = 4000
        } else {
            param.videoResolution = .quality540P
            param.bitrate = 1800
        }
        param.fps = 30
        param.resolutionMode = .portrait
        return param
    }
}

struct VideoAdvanceSetting {
    
    var isVisible: Bool = false
    
    var isUltimateEnabled: Bool = false
    
    var isBFrameEnabled: Bool = false
    
    var isH265Enabled: Bool = false
    
    var hdrRenderType: HDRRenderType = .none
}

enum HDRRenderType: Int {
    case none = 0
    case displayLayer = 1
    case metal = 2
}
