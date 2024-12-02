//
//  SGUserState.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/21.
//

import RTCRoomEngine

struct SGUserState {
    var selfInfo: TUIUserInfo = TUIUserInfo()
    var hasAudioStreamUserList: Set<String> = []
    var speakingUserList: Set<SGUserVolume> = []
}

struct SGUserVolume: Hashable {
    var userId: String
    var volume: Int
    
    static func == (lhs: SGUserVolume, rhs: SGUserVolume) -> Bool {
        lhs.userId == rhs.userId
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(userId)
    }
}
