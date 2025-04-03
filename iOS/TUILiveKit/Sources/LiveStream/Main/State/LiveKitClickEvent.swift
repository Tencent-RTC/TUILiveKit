//
//  LiveKitClickEvent.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/14.
//

import Foundation
import RTCRoomEngine

enum BeautyTypeEvent {
    case closeClick
    case buffingClick
    case whitenessClick
    case ruddyClick
    
    func getString() -> String {
        switch self {
        case .closeClick:
            return .localized("turn off")
        case .buffingClick:
            return .localized("Microdermabrasion")
        case .whitenessClick:
            return .localized("Whitening")
        case .ruddyClick:
            return .localized("Rosy")
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

enum LiveKitClickEvent {
    case `default`
}
