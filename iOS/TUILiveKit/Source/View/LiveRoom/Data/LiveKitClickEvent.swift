//
//  LiveKitClickEvent.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/14.
//

import Foundation
import RTCRoomEngine

enum PrepareViewActionEvent {
    case beautyClick
    case audioEffectsClick
    case flipClick
    case mirrorClick
    case moreSettingClick
    case coverClick
    case categorySelectionClick
    case modeSelectionClick
    case videoQualityValue(_ value: TUIVideoQuality)
    case fpsValue(_ value: Int)
}

enum AudienceLinkActionEvent {
    case videoLinkClick
    case audioLinkClick
    case settingClick
}

enum AudienceVideoSettingActionEvent {
    case beautyClick
    case makeupClick
    case filterClick
    case mirrorClick
    case flipClick
}

enum BeautyTypeEvent {
    case closeClick
    case buffingClick
    case whitenessClick
    case ruddyClick
}

enum LiveKitClickEvent {
    case `default`
}
