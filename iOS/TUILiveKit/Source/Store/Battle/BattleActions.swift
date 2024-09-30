//
//  BattleActions.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/8/26.
//

import Foundation
import RTCRoomEngine
enum BattleActions {
    static let key = "Battle.action"
    
    static let requestBattle = ActionTemplate(id: key.appending(".requestBattle"), payloadType: ([String],TimeInterval).self)
    static let cancelBattleRequest = ActionTemplate(id: key.appending(".cancelBattleRequest"), payloadType: String.self)
    static let acceptBattle = ActionTemplate(id: key.appending("acceptBattle"), payloadType: String.self)
    static let rejectBattle = ActionTemplate(id: key.appending("rejectBattle"), payloadType: String.self)
    static let exitBattle = ActionTemplate(id: key.appending(".exitBattle"), payloadType: String.self)
    
    static let onBattleStarted = ActionTemplate(id: key.appending(".onBattleStarted"), payloadType: TUIBattleInfo.self)
    static let onBattleEnded = ActionTemplate(id: key.appending(".onBattleEnded"), payloadType: (TUIBattleInfo, TUIBattleStoppedReason).self)
    static let onUserJoinBattle = ActionTemplate(id: key.appending(".onUserJoinBattle"), payloadType: TUIBattleUser.self)
    static let onUserExitBattle = ActionTemplate(id: key.appending(".onUserExitBattle"), payloadType: TUIBattleUser.self)
    static let onBattleScoreChanged = ActionTemplate(id: key.appending(".onBattleScoreChanged"), payloadType: [TUIBattleUser].self)
    static let onBattleRequestReceived = ActionTemplate(id: key.appending("onBattleRequestReceived"), payloadType: (String, TUIBattleUser).self)
    static let onBattleRequestCancelled = ActionTemplate(id: key.appending(".onBattleRequestCancelled"), payloadType: TUIBattleUser.self)
    static let onBattleRequestTimeout = ActionTemplate(id: key.appending(".onBattleRequestTimeout"), payloadType: (TUIBattleUser, TUIBattleUser).self)
    static let onBattleRequestAccept = ActionTemplate(id: key.appending(".onBattleRequestAccept"), payloadType: TUIBattleUser.self)
    static let onBattleRequestReject = ActionTemplate(id: key.appending(".onBattleRequestReject"), payloadType: TUIBattleUser.self)
    
    static let addSentBattleInvitation = ActionTemplate(id: key.appending(".addSentBattleInvitation"), payloadType: BattleUser.self)
    static let clearSentBattleInvitation = ActionTemplate(id: key.appending(".clearSentBattleInvitation"))
    static let responseBattleRequest = ActionTemplate(id: key.appending(".responseBattleRequest"))
    
    static let setBattleConfig = ActionTemplate(id: key.appending(".setBattleConfig"), payloadType: TUIBattleConfig.self)
    static let setBattleId = ActionTemplate(id: key.appending(".setBattleId"), payloadType: String.self)
    static let clearBattleUsers = ActionTemplate(id: key.appending(".clearBattleUsers"))
    
    static let setIsInWaiting = ActionTemplate(id: key.appending(".setIsInWaiting"), payloadType: Bool.self)
    static let setIsOnDisplayResult = ActionTemplate(id: key.appending(".setIsOnDisplayResult"), payloadType: Bool.self)
}
