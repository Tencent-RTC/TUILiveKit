//
//  ConnectionActions.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/6.
//

import Foundation
import RTCRoomEngine

enum ConnectionActions {
    static let key = "Connection.action"
    
    static let getRecommendedList = ActionTemplate(id: key.appending(".getRecommendedList"), payloadType: String.self)
    static let updateRecommendedList = ActionTemplate(id: key.appending(".updateRecommendedList"), payloadType: (String, String, [TUILiveInfo]).self)
    
    static let requestConnection = ActionTemplate(id: key.appending(".requestConnection"), payloadType:([String], String).self)
    
    static let cancelRequest = ActionTemplate(id: key.appending(".cancelRequest"), payloadType:[String].self)
    
    static let accept = ActionTemplate(id: key.appending(".accept"), payloadType:String.self)
    static let reject = ActionTemplate(id: key.appending(".reject"), payloadType:String.self)
    
    static let disconnect = ActionTemplate(id: key.appending(".disconnect"))
    
    static let onConnectionUserListChanged = ActionTemplate(id: key.appending(".onConnectionUserListChanged"), payloadType: [TUIConnectionUser].self)
    
    static let onConnectionRequestReceived = ActionTemplate(
        id: key.appending(".onConnectionRequestReceived"),
        payloadType: (TUIConnectionUser, [TUIConnectionUser], String).self)
    static let onConnectionRequestCancelled = ActionTemplate(id: key.appending(".onConnectionRequestCancelled"), payloadType: TUIConnectionUser.self)
    static let onConnectionRequestAccept = ActionTemplate(id: key.appending(".onConnectionRequestAccept"), payloadType: TUIConnectionUser.self)
    static let onConnectionRequestReject = ActionTemplate(id: key.appending(".onConnectionRequestReject"), payloadType: TUIConnectionUser.self)
    static let onConnectionRequestTimeout = ActionTemplate(
        id: key.appending(".onConnectionRequestTimeout"),
        payloadType: (String, TUIConnectionUser, TUIConnectionUser).self)
    
    static let addSentConnectionInvitation = ActionTemplate(id: key.appending(".addSentConnectionInvitation"), payloadType:ConnectionUser.self)
    static let removeSentConnectionInvitation = ActionTemplate(id: key.appending(".removeSentConnectionInvitation"), payloadType:String.self)
    
    static let respondConnectionRequest = ActionTemplate(id: key.appending(".respondConnectionRequest"), payloadType:(Bool, String).self)
}

// MARK: - Subject action, only event, no reduce.
enum ConnectionResponseActions {
    static let key = "Connection.response"
}
