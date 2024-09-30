//
//  VideoRenderModel.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/10.
//

import Foundation

struct VideoRenderModel: Encodable {
    
    var userId: String
    var avatarUrl: String
    var userName: String
    
    var roomId:String
    
    init() {
        userId = ""
        avatarUrl = ""
        userName = ""
        roomId = ""
    }
    
    init(seatInfo: SeatInfo) {
        self.init()
        self.userId = seatInfo.userId
        self.userName = seatInfo.userName
        self.avatarUrl = seatInfo.avatarUrl
    }

    init(connectionUser: ConnectionUser) {
        self.init()
        self.userId = connectionUser.userId
        self.userName = connectionUser.userName
        self.avatarUrl = connectionUser.avatarUrl
        self.roomId = connectionUser.roomId
    }
    
    init(battleUser: BattleUser) {
        self.init()
        self.userId = battleUser.userId
        self.userName = battleUser.userName
        self.avatarUrl = battleUser.avatarUrl
        self.roomId = battleUser.roomId
    }
}

extension VideoRenderModel: Equatable {}

extension VideoRenderModel: Hashable {
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(userId)
    }
    
}
