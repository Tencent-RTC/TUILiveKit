//
//  CoHostManager.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/29.
//

import RTCCommon
import RTCRoomEngine

class CoHostManager {
    let observerState = ObservableState<CoHostState>(initialState: CoHostState())
    var coHostState: CoHostState {
        observerState.state
    }
    
    private weak var context: LiveStreamManager.Context?
    private let service: LiveStreamService
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    func isEnable() -> Bool {
        return coHostState.enableConnection
    }
    
    func setEnableConnection(enable: Bool) {
        modifyCoHostState(value: enable, keyPath: \CoHostState.enableConnection)
    }
    
    func requestConnection(roomId: String, timeout: Int) async throws {
        do {
            let map = try await service.requestConnection(roomIdList: [roomId], timeout: timeout, extensionInfo: "")
            let code = map[roomId]
            if code == .success {
                let connectUser = TUIConnectionUser()
                connectUser.roomId = roomId
                addSendConnectionRequest(user: connectUser)
            } else {
                throw LiveStreamCoreError.error(code: .failed, message: "requestConnection failed")
            }
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func cancelRequest(roomId: String) async throws {
        do {
            try await service.cancelConnectionRequest(list: [roomId])
            removeSendConnectionRequest(inviteeRoomId: roomId)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func accept(roomId: String) async throws {
        do {
            try await service.acceptConnection(roomId: roomId)
            modifyCoHostState(value: nil, keyPath: \CoHostState.receivedConnectionRequest)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func reject(roomId: String) async throws {
        do {
            try await service.rejectConnection(roomId: roomId)
            modifyCoHostState(value: nil, keyPath: \CoHostState.receivedConnectionRequest)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func disconnect() async throws {
        do {
            try await service.disconnect()
            modifyCoHostState(value: [], keyPath: \CoHostState.connectedUserList, isPublished: true)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func onLeaveRoom() {
        modifyCoHostState(value: [], keyPath: \CoHostState.connectedUserList, isPublished: true)
        modifyCoHostState(value: [], keyPath: \CoHostState.sentConnectionRequestList)
        modifyCoHostState(value: nil, keyPath: \CoHostState.receivedConnectionRequest)
    }
}

// MARK: - Observer
extension CoHostManager {
    func onConnectionUserListChanged(connectedList: [TUIConnectionUser], joinedList: [TUIConnectionUser], leavedList: [TUIConnectionUser]) {
        modifyCoHostState(value: connectedList, keyPath: \CoHostState.connectedUserList, isPublished: true)
    }
    
    func onConnectionRequestReceived(inviter: TUIConnectionUser) {
        guard let context = context else { return }
        if context.coGuestManager.coGuestState.connectedUserList.count > 1 {
            Task {
                try await reject(roomId: inviter.roomId)
            }
            return
        }
        modifyCoHostState(value: inviter, keyPath: \CoHostState.receivedConnectionRequest)
    }
    
    func onConnectionRequestCancelled(inviter: TUIConnectionUser) {
        modifyCoHostState(value: nil, keyPath: \CoHostState.receivedConnectionRequest)
    }
    
    func onConnectionRequestAccept(invitee: TUIConnectionUser) {
        removeSendConnectionRequest(inviteeRoomId: invitee.roomId)
    }
    
    func onConnectionRequestReject(invitee: TUIConnectionUser) {
        removeSendConnectionRequest(inviteeRoomId: invitee.roomId)
    }
    
    func onConnectionRequestTimeout(inviter: TUIConnectionUser, invitee: TUIConnectionUser) {
        if inviter.roomId == context?.roomManager.roomState.roomId {
            removeSendConnectionRequest(inviteeRoomId: invitee.roomId)
        } else {
            modifyCoHostState(value: nil, keyPath: \CoHostState.receivedConnectionRequest)
        }
    }
}

// MARK: - Private
extension CoHostManager {
    private func addSendConnectionRequest(user: TUIConnectionUser) {
        observerState.update { coHostState in
            coHostState.sentConnectionRequestList.append(user)
        }
    }
    
    private func modifyCoHostState<T>(value: T, keyPath: WritableKeyPath<CoHostState, T>, isPublished: Bool = false) {
        observerState.update(isPublished: isPublished) { coHostState in
            coHostState[keyPath: keyPath] = value
        }
    }
    
    private func removeSendConnectionRequest(inviteeRoomId: String) {
        observerState.update(isPublished: false) { coHostState in
            coHostState.sentConnectionRequestList.removeAll { user in
                user.roomId == inviteeRoomId
            }
        }
    }
}
