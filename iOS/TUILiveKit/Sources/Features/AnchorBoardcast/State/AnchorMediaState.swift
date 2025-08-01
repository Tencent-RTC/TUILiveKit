//
//  AnchorMediaState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import RTCRoomEngine

struct AnchorMediaState {
    var audioQuality: TUIAudioQuality = .default
    var videoQuality: TUIVideoQuality = .quality1080P
    var isAudioLocked: Bool = false
    var isVideoLocked: Bool = false
    
    var videoAdvanceSettings: AnchorVideoAdvanceSetting = AnchorVideoAdvanceSetting()
}

struct AnchorVideoAdvanceSetting {
    
    var isVisible: Bool = false
    
    var isUltimateEnabled: Bool = false
    
    var isBFrameEnabled: Bool = false
    
    var isH265Enabled: Bool = false
    
    var hdrRenderType: AnchorHDRRenderType = .none
}

enum AnchorHDRRenderType: Int {
    case none = 0
    case displayLayer = 1
    case metal = 2
}
