//
//  ConnectionSelectors.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/6.
//

import Foundation

enum ConnectionSelectors {
    static let getConnectionState = Selector(keyPath: \OperationState.connectionState)
    
    static let getConnectedUsers = Selector(keyPath: \OperationState.connectionState.connectedUsers)
    
    static let isConnecting = Selector.with(getConnectedUsers) { users in
        return users.count > 0
    }

    static let getRecommendedCursor = Selector(keyPath: \OperationState.connectionState.recommendedCursor)
    static let getRecommendedUsers = Selector(keyPath: \OperationState.connectionState.recommendedUsers)

    static let getSentConnectionRequests = Selector(keyPath: \OperationState.connectionState.sentConnectionRequests)

    static let getReceivedConnectionRequest = Selector(keyPath: \OperationState.connectionState.receivedConnectionRequest)
}
