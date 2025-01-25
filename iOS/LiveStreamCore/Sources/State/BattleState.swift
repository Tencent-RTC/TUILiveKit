//
//  BattleState.swift
//  LiveStreamCore
//
//  Created by adamsfliu on 2024/12/19.
//

import RTCRoomEngine

public struct BattleState: State {
    public var isBattleStart: Bool = false
    public var battleId: String = ""
    public var sentBattleRequestList: [TUIBattleUser] = []
    public var battledUsers: [TUIBattleUser] = []
    
    public init() {}
}
