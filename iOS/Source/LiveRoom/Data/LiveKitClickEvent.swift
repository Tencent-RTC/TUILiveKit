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
    case categoryValue(_ value: LiveStreamCategory)
    case modeValue(_ value: LiveMode)
    case videoQualityValue(_ value: TUIVideoQuality)
    case fpsValue(_ value: Int)
}

enum LivingViewActionEvent {
    case linkClick
    case setClick
    case musicClick
    case pkClick
    case moreClick
    case closeClick
}

enum AudienceViewActionEvent {
    case giftClick
    case linkClick
    case willCancelRequestLinkClick
    case didCancelRequestLinkClick
    case willCloseLinkClick
    case didCloseLinkClick
    case likeClick
    case leaveClick
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

enum AnchorSettingActionEvent {
    case beautyClick
    case audioEffectsClick
    case flipClick
    case mirrorClick
    case videoParametersClick
    case moreSettingClick
}

enum AudioEffectEvent {
    case voiceEarMonitorClick
}

enum ChangerTypeEvent {
    case withoutChangerClick
    case childChangerClick
    case girlChangerClick
    case uncleChangerClick
    case etherealChangerClick
}

enum ReverbTypeEvent {
    case withoutReverbClick
    case karaokeReverbClick
    case metalReverbClick
    case lowReverbClick
    case loudReverbClick
}

enum LiveKitClickEvent {
    case `default`
}
