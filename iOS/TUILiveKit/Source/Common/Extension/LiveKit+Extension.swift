//
//  LiveKit+Extension.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/17.
//

import Foundation
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

extension LiveStreamCategory {
    func getString() -> String {
        switch self {
        case .chat:
            return .localized("live.category.chat")
        case .living:
            return .localized("live.category.living")
        case .beauty:
            return .localized("live.category.beauty")
        case .teach:
            return .localized("live.category.teach")
        case .shopping:
            return .localized("live.category.shopping")
        case .music:
            return .localized("live.category.music")
        }
    }
}

extension LiveMode {
    func getString() -> String {
        switch self {
        case .public:
            return .localized("live.mode.public")
        case .privacy:
            return .localized("live.mode.privacy")
        }
    }
}

extension TUIVideoQuality {
    func getString() -> String {
        switch self {
        case .quality360P:
            return .localized("live.video.quality.360P")
        case .quality540P:
            return .localized("live.video.quality.540P")
        case .quality720P:
            return .localized("live.video.quality.720P")
        case .quality1080P:
            return .localized("live.video.quality.1080P")
        @unknown default:
            return .localized("live.video.quality.1080P")
        }
    }
}

extension ChangerTypeEvent {
    func getString() -> String {
        switch self {
        case .withoutChangerClick:
            return .localized("live.music.changer.none")
        case .childChangerClick:
            return .localized("live.music.changer.child")
        case .girlChangerClick:
            return .localized("live.music.changer.girl")
        case .uncleChangerClick:
            return .localized("live.music.changer.uncle")
        case .etherealChangerClick:
            return .localized("live.music.changer.ethereal")
        }
    }

    func getImage() -> UIImage? {
        switch self {
        case .withoutChangerClick:
            return .liveBundleImage("live_audio_none")
        case .childChangerClick:
            return .liveBundleImage("live_audio_changer_child")
        case .girlChangerClick:
            return .liveBundleImage("live_audio_changer_girl")
        case .uncleChangerClick:
            return .liveBundleImage("live_audio_changer_uncle")
        case .etherealChangerClick:
            return .liveBundleImage("live_audio_changer_ethereal")
        }
    }

    func getChangerType() -> TXVoiceChangeType {
        switch self {
        case .withoutChangerClick:
            return ._0
        case .childChangerClick:
            return ._1
        case .girlChangerClick:
            return ._2
        case .uncleChangerClick:
            return ._3
        case .etherealChangerClick:
            return ._11
        }
    }
}

extension ReverbTypeEvent {
    func getString() -> String {
        switch self {
        case .withoutReverbClick:
            return .localized("live.music.reverb.none")
        case .karaokeReverbClick:
            return .localized("live.music.reverb.karaoke")
        case .metalReverbClick:
            return .localized("live.music.reverb.metal")
        case .lowReverbClick:
            return .localized("live.music.reverb.low")
        case .loudReverbClick:
            return .localized("live.music.reverb.loud")
        }
    }

    func getImage() -> UIImage? {
        switch self {
        case .withoutReverbClick:
            return .liveBundleImage("live_audio_none")
        case .karaokeReverbClick:
            return .liveBundleImage("live_audio_reverb_karaoke")
        case .metalReverbClick:
            return .liveBundleImage("live_audio_reverb_metal")
        case .lowReverbClick:
            return .liveBundleImage("live_audio_reverb_low")
        case .loudReverbClick:
            return .liveBundleImage("live_audio_reverb_loud")
        }
    }

    func getReverbType() -> TXVoiceReverbType {
        switch self {
        case .withoutReverbClick:
            return ._0
        case .karaokeReverbClick:
            return ._1
        case .metalReverbClick:
            return ._6
        case .lowReverbClick:
            return ._4
        case .loudReverbClick:
            return ._5
        }
    }
}

extension BeautyTypeEvent {
    func getString() -> String {
        switch self {
        case .closeClick:
            return .localized("live.beauty.close")
        case .buffingClick:
            return .localized("live.beauty.buffing")
        case .whitenessClick:
            return .localized("live.beauty.whiteness")
        case .ruddyClick:
            return .localized("live.beauty.ruddy")
        }
    }

    func getImage() -> UIImage? {
        switch self {
        case .closeClick:
            return .liveBundleImage("live_beauty_close")
        case .buffingClick:
            return .liveBundleImage("live_beauty_buffing")
        case .whitenessClick:
            return .liveBundleImage("live_beauty_whiteness")
        case .ruddyClick:
            return .liveBundleImage("live_beauty_ruddy")
        }
    }
}
