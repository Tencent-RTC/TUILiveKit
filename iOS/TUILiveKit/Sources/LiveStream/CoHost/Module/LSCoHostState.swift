//
//  LSCoHostState.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/6.
//

import Foundation
import RTCRoomEngine

struct LSCoHostState: Encodable {
    let currentRoomId: String
    var connectedUsers: [ConnectionUser] = []
    
    var sentConnectionRequests: [ConnectionUser] = []
    var receivedConnectionRequest: ConnectionUser?
    
    var recommendedUsers: [ConnectionUser] = []
    var recommendedListCursor: String = ""
    
    mutating func updateConnectedUserList(_ connectedUserList: [ConnectionUser]){
        connectedUsers = connectedUserList
        for user in connectedUsers {
            recommendedUsers.removeAll(where: {$0.roomId == user.roomId})
        }
    }

    mutating func addSentConnectionRequest(_ invitee: ConnectionUser) {
        if !sentConnectionRequests.contains(where: {$0.roomId == invitee.roomId}) {
            sentConnectionRequests.append(invitee)
        }
        if let index = recommendedUsers.firstIndex(where: {$0.roomId == invitee.roomId}) {
            recommendedUsers[index].connectionStatus = .inviting
        }
    }
    
    mutating func removeSentConnectionRequest(_ inviteeRoomId: String) {
        sentConnectionRequests.removeAll(where: {$0.roomId == inviteeRoomId})
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

struct ConnectionUser: Codable {
    var roomId: String

    var userId: String
    var avatarUrl: String
    var userName: String
    
    var joinConnectionTime: UInt = 0
    var connectionStatus: TUIConnectionStatus = .none
    var connectionExtensionInfo: String = ""
    
    init() {
        roomId = ""
        userId = ""
        avatarUrl = ""
        userName = ""
    }
    
    init(_ connectionUser: TUIConnectionUser, connectionCode:TUIConnectionCode = .unknown) {
        self.roomId = connectionUser.roomId
        self.userId = connectionUser.userId
        self.userName = connectionUser.userName
        self.avatarUrl = connectionUser.avatarUrl
        if connectionCode == .connecting {
            self.connectionStatus = .inviting
        }
    }
    
    init(_ liveInfo: TUILiveInfo) {
        self.roomId = liveInfo.roomInfo.roomId
        self.userId = liveInfo.roomInfo.ownerId
        self.userName = liveInfo.roomInfo.ownerName
        self.avatarUrl = liveInfo.roomInfo.ownerAvatarUrl
        self.joinConnectionTime = 0
    }
}

extension ConnectionUser: Equatable {}

extension ConnectionUser: Hashable {
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(roomId)
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

