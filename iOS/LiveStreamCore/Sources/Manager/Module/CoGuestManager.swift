//
//  CoGuestManager.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/30.
//

import RTCCommon
import RTCRoomEngine

class CoGuestManager {
    let observerState = ObservableState<CoGuestState>(initialState: CoGuestState())
    var coGuestState: CoGuestState {
        observerState.state
    }
    
    private weak var context: LiveStreamManager.Context?
    private let service: LiveStreamService
    
    private let requestDefaultIndex = -1
    private let requestTimeOut = 60
    private var sentInvitationMap: [String: TUIRequest] = [:]
    private var receivedInvitation: TUIRequest?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    func isEnable() -> Bool {
        return coGuestState.enableConnection
    }
    
    func setEnableConnection(enable: Bool) {
        modifyCoGuestState(value: enable, keyPath: \CoGuestState.enableConnection)
    }
    
    func initConnectedGuestList() async throws {
        do {
            let list = try await service.getSeatList()
            initSeatList(list: list)
            updateSelfSeatedState()
            autoTakeSeatByOwner()
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func initGuestApplicationList() async throws {
        do {
            let list = try await service.getSeatApplicationList()
            initSeatApplicationList(list: list)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func applyToConnection(timeOut: Int) async throws -> TakeSeatResult {
        if coGuestState.coGuestStatus == .linking {
            throw LiveStreamCoreError.error(code: .alreadyInSeat,
                                            message: TUIError.alreadyInSeat.lcDescription)
        }
        modifyCoGuestState(value: .applying, keyPath: \CoGuestState.coGuestStatus, isPublished: true)
        do {
            let result = try await service.takeSeat(seatIndex: requestDefaultIndex,
                                                    timeout: TimeInterval(timeOut)) { [weak self] request in
                self?.modifyCoGuestState(value: request.requestId, keyPath: \CoGuestState.myRequestId)
            }
            switch result {
            case .accepted(userId: let userId):
                modifyCoGuestState(value: "", keyPath: \CoGuestState.myRequestId)
                if context?.roomManager.roomState.ownerInfo.userId != context?.userManager.userState.selfInfo.userId {
                    modifyCoGuestState(value: .linking, keyPath: \CoGuestState.coGuestStatus, isPublished: true)
                    onUserConnectionAccepted(userId: userId)
                }
            case .rejected(userId: let userId):
                clearMyRequest()
                onUserConnectionRejected(userId: userId)
            case .timeout(userId: let userId):
                clearMyRequest()
                onUserConnectionTimeout(userId: userId)
            case .cancel(userId: _):
                clearMyRequest()
            }
            return result
        } catch let LiveStreamCoreError.seatError(userId, code, message) {
            clearMyRequest()
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.seatError(userId: userId, code: code, message: message)
        }
    }
    
    func inviteGuestToConnection(userId: String) async throws {
        do {
            let result = try await service.takeUserOnSeatByAdmin(seatIndex: requestDefaultIndex, userId: userId, timeout: TimeInterval(requestTimeOut)) { [weak self] request in
                self?.sentInvitationMap[userId] = request
            }
            switch result {
            case .accepted(userId: let userId):
                onUserConnectionAccepted(userId: userId)
            case .rejected(userId: let userId):
                onUserConnectionRejected(userId: userId)
            case .timeout(userId: let userId):
                onUserConnectionTimeout(userId: userId)
            default: break
            }
            sentInvitationMap.removeValue(forKey: userId)
        } catch let LiveStreamCoreError.seatError(userId, code, message) {
            sentInvitationMap.removeValue(forKey: userId)
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.seatError(userId: userId, code: code, message: message)
        }
    }
    
    func respondGuestApplication(userId: String, isAgree: Bool) async throws {
        guard let requestId = getAudienceRequestIdByUserId(userId: userId) else {
            throw LiveStreamCoreError.error(code: .failed, message: "Invalid requestId")
        }
        do {
            try await service.responseRemoteRequest(requestId: requestId, agree: isAgree)
            removeSeatApplication(requestId: requestId)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func respondGuestInvitation(isAgree: Bool) async throws {
        guard let requestId = receivedInvitation?.requestId else {
            throw LiveStreamCoreError.error(code: .failed, message: "Invalid requestId")
        }
        do {
            try await service.responseRemoteRequest(requestId: requestId, agree: isAgree)
            receivedInvitation = nil
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func cancelGuestApplication() async throws {
        let requestId = coGuestState.myRequestId
        guard !requestId.isEmpty else { return }
        do {
            try await service.cancelRequest(requestId: requestId)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func cancelInviteApplication(userId: String) async throws {
        guard let requestId = sentInvitationMap[userId]?.requestId else { return }
        do {
            try await service.cancelRequest(requestId: requestId)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func disconnectBySelf() async throws {
        do {
            try await service.leaveSeat()
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func disconnectByAdmin(userId: String) async throws {
        do {
            try await service.kickUserOffSeatByAdmin(seatIndex: requestDefaultIndex, userId: userId)
        } catch let LiveStreamCoreError.error(code, message) {
            LiveStreamLog.error("\(#file)","\(#line)","leave:[code:\(code),message:\(message)]")
            throw LiveStreamCoreError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func enableAutoOpenCameraOnSeated(enable: Bool) {
        modifyCoGuestState(value: enable, keyPath: \CoGuestState.openCameraOnCoGuest)
    }
    
    func onLeaveRoom() {
        modifyCoGuestState(value: .none, keyPath: \CoGuestState.coGuestStatus, isPublished: true)
        modifyCoGuestState(value: [], keyPath: \CoGuestState.connectionRequestList)
        modifyCoGuestState(value: "", keyPath: \CoGuestState.myRequestId)
        modifyCoGuestState(value: [], keyPath: \CoGuestState.connectedUserList, isPublished: true)
    }
}

// MARK: - Observer
extension CoGuestManager {
    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        initSeatList(list: seatList)
        for seatInfo in seatedList {
            if isSelfInfo(seatInfo: seatInfo) {
                modifyCoGuestState(value: .linking, keyPath: \CoGuestState.coGuestStatus, isPublished: true)
            }
        }
        for seatInfo in leftList {
            if isSelfInfo(seatInfo: seatInfo) {
                modifyCoGuestState(value: .none, keyPath: \CoGuestState.coGuestStatus, isPublished: true)
            }
        }
    }
    
    func onRequestReceived(request: TUIRequest) {
        guard let context = context else { return }
        let coHostState = context.coHostManager.coHostState
        if !coHostState.connectedUserList.isEmpty ||
            !coHostState.sentConnectionRequestList.isEmpty ||
            coHostState.receivedConnectionRequest != nil {
            Task {
                try await service.responseRemoteRequest(requestId: request.requestId, agree: false)
            }
            return
        }
        
        guard !isRequestInvalid(request: request) else { return }
        switch request.requestAction {
        case .takeSeat:
            addSeatApplication(request: request)
        case .remoteUserOnSeat:
            receivedInvitation = request
        default:
            break;
        }
    }
    
    func onRequestCancelled(request: TUIRequest, operateUser: TUIUserInfo) {
        if request.requestAction == .takeSeat {
            removeSeatApplication(requestId: request.requestId)
        }
    }
    
    func onRequestProcessed(request: TUIRequest, operateUser: TUIUserInfo) {
        removeSeatApplication(requestId: request.requestId)
    }
}

// MARK: - Private
extension CoGuestManager {
    private func initSeatList(list: [TUISeatInfo]) {
        let newList = list.filter { info in
            if let userId = info.userId, !userId.isEmpty {
                return true
            }
            return false
        }
        modifyCoGuestState(value: newList, keyPath: \CoGuestState.connectedUserList, isPublished: true)
    }
    
    private func initSeatApplicationList(list: [TUIRequest]) {
        observerState.update(isPublished: false) { coGuestState in
            coGuestState.connectionRequestList.removeAll()
            coGuestState.connectionRequestList.formUnion(list)
        }
    }
    
    private func updateSelfSeatedState() {
        if isSelfInSeat() {
            modifyCoGuestState(value: .linking, keyPath: \CoGuestState.coGuestStatus, isPublished: true)
        }
    }
    
    private func isSelfInSeat() -> Bool {
        guard let context = context else { return false }
        let selfUserId = context.userManager.userState.selfInfo.userId
        if selfUserId.isEmpty {
            return false
        }
        for seatInfo in coGuestState.connectedUserList {
            if selfUserId == seatInfo.userId {
                return true
            }
        }
        return false
    }
    
    private func autoTakeSeatByOwner() {
        guard context?.userManager.userState.selfInfo.userRole == .roomOwner else { return }
        if coGuestState.coGuestStatus != .linking {
            Task {
                try await applyToConnection(timeOut: requestTimeOut)
            }
        }
    }
    
    private func getAudienceRequestIdByUserId(userId: String) -> String? {
        return coGuestState.connectionRequestList.first { $0.userId == userId }?.requestId
    }
    
    private func removeSeatApplication(requestId: String) {
        if let requestToRemove = coGuestState.connectionRequestList.first(where: { $0.requestId == requestId }) {
            observerState.update(isPublished: false) { coGuestState in
                coGuestState.connectionRequestList.remove(requestToRemove)
            }
        }
    }
    
    private func clearMyRequest() {
        modifyCoGuestState(value: .none, keyPath: \CoGuestState.coGuestStatus, isPublished: true)
        modifyCoGuestState(value: "", keyPath: \CoGuestState.myRequestId)
    }
    
    private func isSelfInfo(seatInfo: TUISeatInfo) -> Bool {
        guard let context = context else { return false }
        if context.userManager.userState.selfInfo.userId.isEmpty {
            return false
        }
        return seatInfo.userId == context.userManager.userState.selfInfo.userId
    }
    
    private func addSeatApplication(request: TUIRequest) {
        observerState.update(isPublished: false) { coGuestState in
            coGuestState.connectionRequestList.insert(request)
        }
    }
    
    private func isRequestInvalid(request: TUIRequest) -> Bool {
        if request.userId.isEmpty {
            return true
        }
        return request.requestId.isEmpty
    }
    
    private func onUserConnectionAccepted(userId: String) {
        let userInfo = TUIUserInfo()
        userInfo.userId = userId
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onUserConnectionAccepted(userInfo: userInfo)
            }
        }
    }
    
    private func onUserConnectionRejected(userId: String) {
        let userInfo = TUIUserInfo()
        userInfo.userId = userId
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onUserConnectionRejected(userInfo: userInfo)
            }
        }
    }
    
    private func onUserConnectionTimeout(userId: String) {
        let userInfo = TUIUserInfo()
        userInfo.userId = userId
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onUserConnectionTimeout(userInfo: userInfo)
            }
        }
    }
    
    private func modifyCoGuestState<T>(value: T, keyPath: WritableKeyPath<CoGuestState, T>, isPublished: Bool = false) {
        observerState.update(isPublished: isPublished) { coGuestState in
            coGuestState[keyPath: keyPath] = value
        }
    }
}
