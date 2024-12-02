//
//  Modules.swift
//  AFNetworking
//
//  Created by aby on 2024/11/18.
//

import Foundation
import RTCRoomEngine
import RTCCommon
import Combine

class LSCoHostManager {

    let toastSubject: PassthroughSubject<String, Never>
    
    private let observableState: ObservableState<LSCoHostState>
    var state: LSCoHostState {
        observableState.state
    }
    
    private var listCount = 20
    
    private let service: LSCoHostService
    private typealias Context = LiveStreamManager.Context
    init(context: LiveStreamManager.Context ) {
        self.service = context.coHostService
        self.toastSubject = context.toastSubject
        let roomId = context.roomManager.roomState.roomId
        self.observableState = ObservableState(initialState: LSCoHostState(currentRoomId: roomId))
    }
    
    func subscribeCoHostState<Value>(_ selector: StateSelector<LSCoHostState, Value>) -> AnyPublisher<Value, Never> {
        return observableState.subscribe(selector)
    }
    
    func isCoHostConnecting() -> Bool {
        return !state.connectedUsers.isEmpty
    }
    
    func fetchRecommendedList(cursor: String = "") {
        Task {
            do {
                let cursor = state.recommendedListCursor
                let result = try await service.fetchRecommendedList(cursor: cursor, count: listCount)
                let responseCursor = result.0
                let liveList = result.1
                observableState.update { state in
                    if cursor.isEmpty {
                        state.recommendedUsers.removeAll()
                    }
                    let recommendedUsers: [ConnectionUser] = liveList.map { liveInfo in
                        let isConnected = state.connectedUsers.contains(where: { $0.roomId == liveInfo.roomInfo.roomId })
                        if !isConnected {
                            var user = ConnectionUser(liveInfo)
                            if state.sentConnectionRequests.contains(where: { $0.roomId == user.roomId }) {
                                user.connectionStatus = .inviting
                            }
                            return user
                        } else {
                            return ConnectionUser()
                        }
                    }.filter { !$0.roomId.isEmpty }
                    state.recommendedUsers.append(contentsOf: recommendedUsers)
                    state.recommendedListCursor = responseCursor
                }
            } catch {
            }
        }
    }
    
    func onRequestConnection(user: ConnectionUser) {
        for recommendedUser in state.recommendedUsers {
            if recommendedUser.roomId == user.roomId {
                var useUser = recommendedUser
                useUser.connectionStatus = .inviting
                observableState.update { state in
                    state.addSentConnectionRequest(useUser)
                }
            }
        }
    }
    
    func onAccept() {
        observableState.update { state in
            state.receivedConnectionRequest = nil
        }
    }
    
    func onReject() {
        observableState.update { state in
            state.receivedConnectionRequest = nil
        }
    }
}

extension LSCoHostManager {
    func update(connectedUser: [ConnectionUser]) {
        observableState.update { state in
            state.connectedUsers = connectedUser
        }
    }
}

extension LSCoHostManager {
    func onConnectionUserListChanged(list: [TUIConnectionUser]) {
        observableState.update { state in
            let users = list.map { user in
                var connectionUser = ConnectionUser(user)
                connectionUser.connectionStatus = .connected
                return connectionUser
            }
            state.updateConnectedUserList(users)
        }
    }
    
    func onConnectionRequestReceived(inviter: TUIConnectionUser, inviteeList: [TUIConnectionUser], extensionInfo: String) {
        observableState.update { state in
            var inviter = ConnectionUser(inviter)
            inviter.connectionStatus = .inviting
            state.receivedConnectionRequest = inviter
        }
    }
    
    func onConnectionRequestCancelled(inviter: TUIConnectionUser) {
        observableState.update { state in
            state.receivedConnectionRequest = nil
        }
    }
    
    func onConnectionRequestAccept(invitee: TUIConnectionUser) {
        let roomId = invitee.roomId
        observableState.update { state in
            state.removeSentConnectionRequest(roomId)
        }
    }
    
    func onConnectionRequestTimeout(inviter: TUIConnectionUser, invitee: TUIConnectionUser) {
        observableState.update { state in
            if inviter.roomId == state.currentRoomId {
                state.removeSentConnectionRequest(invitee.roomId)
            } else {
                state.receivedConnectionRequest = nil
            }
        }
    }
    
    func onConnectionRequestReject(invitee: TUIConnectionUser) {
        let roomId = invitee.roomId
        observableState.update { state in
            state.removeSentConnectionRequest(roomId)
        }
    }
}

fileprivate extension String {
    static let operationSuccessful = localized("live.error.success")
    
    static let errorConnectionDisableText = localized("live.error.connectionDisable.connecting")
    static let errorConnectionMaxLimitText = localized("live.error.connectionDisable.maxLimit")
}
