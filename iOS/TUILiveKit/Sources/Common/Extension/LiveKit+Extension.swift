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

extension LiveStreamPrivacyStatus {
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
            return "360P"
        case .quality540P:
            return "540P"
        case .quality720P:
            return "720P"
        case .quality1080P:
            return "1080P"
        @unknown default:
            return "1080P"
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
