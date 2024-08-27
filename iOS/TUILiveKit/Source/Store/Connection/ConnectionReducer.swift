//
//  ConnectionReducer.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/6.
//

import Foundation
import RTCRoomEngine

let connectionReducer = Reducer<ConnectionState>(
    
    ReduceOn(ConnectionActions.updateRecommendedList, reduce: { state, action in
        let requestCursor = action.payload.0
        let responseCursor = action.payload.1
        let responseLiveList = action.payload.2
        
        if (requestCursor == "") {
            state.recommendedUsers.removeAll()
        }
        
        var recommendedUsers: [ConnectionUser] = []
        for liveInfo in responseLiveList {
            let isConnected = state.connectedUsers.contains(where: {$0.roomId == liveInfo.roomInfo.roomId})
            if !isConnected {
                var user: ConnectionUser = ConnectionUser(liveInfo)
                if state.sentConnectionRequests.contains(where: {$0.roomId == user.roomId}) {
                    user.connectionStatus = .inviting
                }
                recommendedUsers.append(user)
            }
        }
        state.recommendedUsers.append(contentsOf: recommendedUsers)
        state.recommendedCursor = responseCursor
    }),

    ReduceOn(ConnectionActions.onConnectionUserListChanged, reduce: { state, action in
        let connectedUsers = action.payload.map { user in
            var connectionUser = ConnectionUser(user)
            connectionUser.connectionStatus = .connected
            return connectionUser
        }
        state.updateConnectedUserList(connectedUsers)
    }),

    ReduceOn(ConnectionActions.onConnectionRequestReceived, reduce: { state, action in
        var inviter = ConnectionUser(action.payload.0)
        inviter.connectionStatus = .inviting
        state.receivedConnectionRequest = inviter
    }),
    ReduceOn(ConnectionActions.onConnectionRequestCancelled, reduce: { state, action in
        state.receivedConnectionRequest = nil
    }),
    ReduceOn(ConnectionActions.onConnectionRequestAccept, reduce: { state, action in
        let invitee = ConnectionUser(action.payload)
        state.removeSentConnectionRequest(invitee.roomId)
    }),
    ReduceOn(ConnectionActions.onConnectionRequestReject, reduce: { state, action in
        let invitee = ConnectionUser(action.payload)
        state.removeSentConnectionRequest(invitee.roomId)
    }),
    ReduceOn(ConnectionActions.onConnectionRequestTimeout, reduce: { state, action in
        let currentRoomId = action.payload.0
        let inviter = action.payload.1
        let invitee = action.payload.2
        if inviter.roomId == currentRoomId {
            state.removeSentConnectionRequest(invitee.roomId)
        } else {
            state.receivedConnectionRequest = nil
        }
    }),
 
    ReduceOn(ConnectionActions.addSentConnectionInvitation, reduce: { state, action in
        state.addSentConnectionRequest(action.payload)
    }),
    ReduceOn(ConnectionActions.removeSentConnectionInvitation, reduce: { state, action in
        state.removeSentConnectionRequest(action.payload)
    }),
    
    ReduceOn(ConnectionActions.respondConnectionRequest, reduce: { state, action in
        state.receivedConnectionRequest = nil
    })

)
