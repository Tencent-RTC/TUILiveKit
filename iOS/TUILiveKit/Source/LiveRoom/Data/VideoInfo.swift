//
//  VideoInfo.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/11.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import RTCRoomEngine
#if TXLiteAVSDK_TRTC
    import TXLiteAVSDK_TRTC
#elseif TXLiteAVSDK_Professional
    import TXLiteAVSDK_Professional
#endif

class VideoInfo {
    var fps: Int {
        switch videoQuality.value {
        case .quality360P:
            return 12
        case .quality540P:
            return 15
        case .quality720P:
            return 15
        case .quality1080P:
            return 20
        default:
            return 20
        }
    }

    var resolution: TRTCVideoResolution {
        switch videoQuality.value {
        case .quality360P:
            return ._640_360
        case .quality540P:
            return ._960_540
        case .quality720P:
            return ._1280_720
        case .quality1080P:
            return ._1920_1080
        default:
            return ._1920_1080
        }
    }

    var bitrate: Int {
        switch videoQuality.value {
        case .quality360P:
            return 800
        case .quality540P:
            return 1_200
        case .quality720P:
            return 2_000
        case .quality1080P:
            return 3_000
        default:
            return 3_000
        }
    }

    let isMirror: Observable<Bool> = Observable(true)
    let isFrontCamera: Observable<Bool> = Observable(true)
    let isCameraOpened: Observable<Bool> = Observable(false)
    let videoQuality: Observable<TUIVideoQuality> = Observable(.quality1080P)
}
