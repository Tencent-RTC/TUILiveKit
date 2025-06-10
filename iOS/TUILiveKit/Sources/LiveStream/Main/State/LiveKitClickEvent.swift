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
            return internalLocalized("turn off")
        case .buffingClick:
            return internalLocalized("Microdermabrasion")
        case .whitenessClick:
            return internalLocalized("Whitening")
        case .ruddyClick:
            return internalLocalized("Rosy")
        }
    }

    func getImage() -> UIImage? {
        switch self {
        case .closeClick:
            return internalImage("live_beauty_close")
        case .buffingClick:
            return internalImage("live_beauty_buffing")
        case .whitenessClick:
            return internalImage("live_beauty_whiteness")
        case .ruddyClick:
            return internalImage("live_beauty_ruddy")
        }
    }
}

enum LiveKitClickEvent {
    case `default`
}
