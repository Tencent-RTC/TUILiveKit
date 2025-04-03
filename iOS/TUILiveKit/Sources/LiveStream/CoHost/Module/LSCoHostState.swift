//
//  LSCoHostState.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/6.
//

import Foundation
import RTCRoomEngine

struct LSCoHostState {
    var currentRoomId: String = ""
    var connectedUsers: [TUIConnectionUser] = []
    
    var recommendedUsers: [TUIConnectionUser] = []
    var recommendedListCursor: String = ""
    
    mutating func updateConnectedUserList(_ connectedUserList: [TUIConnectionUser]){
        connectedUsers = connectedUserList
        for user in connectedUsers {
            recommendedUsers.removeAll(where: {$0.roomId == user.roomId})
        }
    }

    mutating func addSentConnectionRequest(_ invitee: TUIConnectionUser) {
        if let index = recommendedUsers.firstIndex(where: {$0.roomId == invitee.roomId}) {
            recommendedUsers[index].connectionStatus = .inviting
        }
    }
    
    mutating func removeSentConnectionRequest(_ inviteeRoomId: String) {
        if let index = recommendedUsers.firstIndex(where: {$0.roomId == inviteeRoomId}) {
            recommendedUsers[index].connectionStatus = .none
        }
    }
}

enum TUIConnectionStatus: Int {
    case none
    case inviting
    case connected
}

private var connectionStatusKey: UInt = 0
extension TUIConnectionUser {
    var connectionStatus: TUIConnectionStatus {
        get {
            return objc_getAssociatedObject(self, &connectionStatusKey) as? TUIConnectionStatus ?? .none
        }
        set {
            objc_setAssociatedObject(self, &connectionStatusKey, newValue, .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
        }
    }
}

extension TUIConnectionUser {
    convenience init(_ liveInfo: TUILiveInfo) {
        self.init()
        self.roomId = liveInfo.roomInfo.roomId
        self.userId = liveInfo.roomInfo.ownerId
        self.userName = liveInfo.roomInfo.ownerName
        self.avatarUrl = liveInfo.roomInfo.ownerAvatarUrl
        self.joinConnectionTime = 0
    }
}

extension TUIConnectionStatus: Codable {
    public init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let rawValue = try container.decode(Int.self)
        self = TUIConnectionStatus(rawValue: rawValue) ?? .none
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(self.rawValue)
    }
}

