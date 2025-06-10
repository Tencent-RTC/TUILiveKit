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
import TUILiveResources

class LSCoHostManager {

    let toastSubject: PassthroughSubject<String, Never>
    
    private let observableState: ObservableState<LSCoHostState>
    var state: LSCoHostState {
        observableState.state
    }
    
    private var listCount = 20
    private let service: LSCoHostService
    private typealias Context = LiveStreamManager.Context
    private weak var context: Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.coHostService
        self.toastSubject = context.toastSubject
        self.observableState = ObservableState(initialState: LSCoHostState())
        observableState.update { state in
            state.currentRoomId = context.roomManager.roomState.roomId
        }
    }
}

// MARK: - Common
extension LSCoHostManager {
    func onError(_ error: InternalError) {
        toastSubject.send(error.localizedMessage)
    }
    
    func isCoHostConnecting() -> Bool {
        return !state.connectedUsers.isEmpty
    }
    
    func fetchRecommendedList(cursor: String = "") {
        guard let context = context else { return }
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
                    let recommendedUsers: [TUIConnectionUser] = liveList.map { liveInfo in
                        let isConnected = state.connectedUsers.contains(where: { $0.roomId == liveInfo.roomInfo.roomId })
                        if !isConnected {
                            let user = TUIConnectionUser(liveInfo)
                            if context.coreCoHostState.sentConnectionRequestList.contains(where: { $0.roomId == user.roomId }) {
                                user.connectionStatus = .inviting
                            }
                            return user
                        } else {
                            return TUIConnectionUser()
                        }
                    }.filter { !$0.roomId.isEmpty }
                    state.recommendedUsers.append(contentsOf: recommendedUsers)
                    state.recommendedListCursor = responseCursor
                }
            } catch {
            }
        }
    }
    
    func subscribeCoHostState<Value>(_ selector: StateSelector<LSCoHostState, Value>) -> AnyPublisher<Value, Never> {
        return observableState.subscribe(selector)
    }
}

// MARK: - Anchor
extension LSCoHostManager {
    func onCrossRoomConnectionTerminated() {
        update(connectedUser: [])
    }
    
    func onRequestConnection(user: TUIConnectionUser) {
        for recommendedUser in state.recommendedUsers {
            if recommendedUser.roomId == user.roomId {
                let useUser = recommendedUser
                useUser.connectionStatus = .inviting
                observableState.update { state in
                    state.addSentConnectionRequest(useUser)
                }
            }
        }
    }
    
    func onRequestConnectionFailed(roomId: String) {
        for recommendedUser in state.recommendedUsers {
            if recommendedUser.roomId == roomId {
                let useUser = recommendedUser
                useUser.connectionStatus = .none
                observableState.update { state in
                    state.removeSentConnectionRequest(roomId)
                }
            }
        }
    }
}

// MARK: - Observer
extension LSCoHostManager {
    func onConnectionUserListChanged(list: [TUIConnectionUser]) {
        observableState.update { state in
            let users = list.map { user in
                user.connectionStatus = .connected
                return user
            }
            state.updateConnectedUserList(users)
        }
    }
    
    func onConnectionRequestReceived(inviter: TUIConnectionUser, inviteeList: [TUIConnectionUser], extensionInfo: String) {
        observableState.update { state in
            inviter.connectionStatus = .inviting
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
            }
        }
    }
    
    func onConnectionRequestReject(invitee: TUIConnectionUser) {
        let roomId = invitee.roomId
        observableState.update { state in
            state.removeSentConnectionRequest(roomId)
        }
        toastSubject.send(.requestRejectedText)
    }
}

// MARK: - Private functions
extension LSCoHostManager {
    private func update(connectedUser: [TUIConnectionUser]) {
        observableState.update { state in
            state.connectedUsers = connectedUser
        }
    }
}

fileprivate extension String {
    static let requestRejectedText = internalLocalized("Connection application has been rejected")
}
