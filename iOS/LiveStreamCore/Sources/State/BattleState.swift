//
//  BattleState.swift
//  LiveStreamCore
//
//  Created by adamsfliu on 2024/12/19.
//

import RTCRoomEngine

struct BattleState {
    var isBattleStart: Bool = false
    var battleId: String = ""
    var sentBattleRequestList: [TUIBattleUser] = []
    var battledUsers: [TUIBattleUser] = []
}
