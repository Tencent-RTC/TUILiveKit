//
//  SGSeatManager.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/16.
//  Updated by abyyxwang on 2024/11/7.
//

import Foundation
import RTCRoomEngine
import RTCCommon

enum SGTakeSeatResultWithUser {
    case accepted(userInfo: TUIUserInfo)
    case rejected(userInfo: TUIUserInfo)
    case timeout(userInfo: TUIUserInfo)
    case cancel(userInfo: TUIUserInfo)
}

class SGSeatManager {
    
    let observerState = ObservableState<SGSeatState>(initialState: SGSeatState())
    var seatState: SGSeatState {
        observerState.state
    }
    
    private typealias Context = SeatGridViewManager.Context
    private weak var context: Context?
    private var service: SeatGridViewInterface
    private var receivedApplicationMap: [String: TUIRequest] = [:]
    private var sentInvitationMap: [String: TUIRequest] = [:]
    private var receivedInvitation: TUIRequest?
    
    init(context: SeatGridViewManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
    
    func onEnterRoom(roomInfo: TUIRoomInfo) async throws {
        do {
            try await refreshSeatList()
        } catch let SeatGridViewError.error(code, message) {
            VRLog.error("\(#file)","\(#line)",
                                  "SeatManager.onEnterRoom:[code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func takeSeat(index: Int,
                  timeout: Int) async throws -> SGTakeSeatResultWithUser {
        do {
            let closure: SentRequestResultClosure = { [weak self] request in
                guard let self = self else { return }
                self.addSentSeatApplication(request: request)
            }
            let result = try await service.takeSeat(seatIndex: index, timeout: TimeInterval(timeout), requestCallback: closure)
            switch result {
                case .accepted(_, let userId):
                    let userInfo = try await self.service.getUserInfo(userId: userId)
                    return .accepted(userInfo: userInfo)
                case .rejected(_, let userId):
                    let userInfo = try await self.service.getUserInfo(userId: userId)
                    return .rejected(userInfo: userInfo)
                case .cancel(_, let userId):
                    let userInfo = try await self.service.getUserInfo(userId: userId)
                    return .cancel(userInfo: userInfo)
                case .timeout(_, let userId):
                    let userInfo = try await self.service.getUserInfo(userId: userId)
                    return .timeout(userInfo: userInfo)
            }
        } catch let SeatGridViewError.seatErrorWithId(_, userId, code, message) {
            VRLog.error("\(#file)","\(#line)",
                                  "takeSeat:[index:\(index),timeout:\(timeout),code:\(code),message:\(message)]")
            let user = try await self.service.getUserInfo(userId: userId)
            throw SeatGridViewError.seatErrorWithUser(user: user, code: code, message: message)
        }
    }
    
    func moveToSeat(index: Int) async throws {
        do {
            try await self.service.moveSeat(index: index)
        } catch let SeatGridViewError.error(code, message) {
            VRLog.error("\(#file)","\(#line)",
                                  "moveToSeat:[index:\(index),code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func leaveSeat() async throws {
        do {
            try await self.service.leaveSeat()
        } catch let SeatGridViewError.error(code, message) {
            VRLog.error("\(#file)","\(#line)",
                                  "leaveSeat:[code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func lockSeat(index: Int, lockMode: TUISeatLockParams) async throws {
        do {
            try await self.service.lockSeat(index: index, lockMode: lockMode)
        } catch let SeatGridViewError.error(code, message) {
            VRLog.error("\(#file)","\(#line)",
                                  "lockSeat:[code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func takeUserOnSeatByAdmin(index: Int,
                               userId: String,
                               timeout: Int) async throws -> SGTakeSeatResultWithUser {
        do {
            let closure: SentRequestResultClosure = { [weak self] request in
                guard let self = self else { return }
                self.addSentSeatInvitation(request: request)
            }
            let result = try await service.takeUserOnSeatByAdmin(seatIndex: index, userId: userId, timeout: TimeInterval(timeout), requestCallback: closure)
            
            switch result {
                case .accepted(_, let userId):
                    self.removeSentSeatInvitation(userId: userId)
                    let userInfo = try await self.service.getUserInfo(userId: userId)
                    return .accepted(userInfo: userInfo)
                case .rejected(_, let userId):
                    self.removeSentSeatInvitation(userId: userId)
                    let userInfo = try await self.service.getUserInfo(userId: userId)
                    return .rejected(userInfo: userInfo)
                case .cancel(_, let userId):
                    self.removeSentSeatInvitation(userId: userId)
                    let userInfo = try await self.service.getUserInfo(userId: userId)
                    return .cancel(userInfo: userInfo)
                case .timeout(_, let userId):
                    self.removeSentSeatInvitation(userId: userId)
                    let userInfo = try await self.service.getUserInfo(userId: userId)
                    return .timeout(userInfo: userInfo)
            }
        } catch let SeatGridViewError.seatErrorWithId(_, userId, code, message) {
            VRLog.error("\(#file)","\(#line)",
                                  "takeUserOnSeatByAdmin:[code:\(code),message:\(message)]")
            self.removeSentSeatInvitation(userId: userId)
            let user = try await self.service.getUserInfo(userId: userId)
            throw SeatGridViewError.seatErrorWithUser(user: user, code: code, message: message)
        }
    }
    
    func kickUserOffSeatByAdmin(userId: String) async throws {
        do {
            try await self.service.kickUserOffSeatByAdmin(userId: userId)
        } catch let SeatGridViewError.error(code, message) {
            VRLog.error("\(#file)","\(#line)",
                                  "kickUserOffSeatByAdmin:[code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func refreshSeatList() async throws {
        do {
            let seatInfoList = try await self.service.getSeatList()
            observerState.update { seatState in
                seatState.seatList = seatInfoList
            }
        } catch let SeatGridViewError.error(code, message) {
            VRLog.error("\(#file)","\(#line)",
                                  "getSeatList:[code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    func responseRemoteRequest(userId: String, agree: Bool) async throws {
        var requestId = ""
        var requestType: SGRequestType? = nil
        if let request = receivedApplicationMap[userId], !request.requestId.isEmpty {
            requestId = request.requestId
            requestType = .applyToTakeSeat
        }
        if let request = receivedInvitation, !request.requestId.isEmpty {
            requestId = request.requestId
            requestType = .inviteToTakeSeat
        }
        guard !requestId.isEmpty else {
            throw SeatGridViewError.error(code: TUIError.failed.rawValue, message: "Invalid userId, don't find any Application or Invatation")
        }
        
        do {
            try await service.responseRemoteRequest(requestId: requestId, agree: agree)
            guard let type = requestType else  { return }
            type == .applyToTakeSeat ?  removeSeatApplication(userId: userId) : removeReceivedSeatInvitation()
        } catch let SeatGridViewError.error(code, message) {
            throw SeatGridViewError.error(code: code, message: message)
        }
    }
    
    func cancelRequest(userId: String) async throws {
        if let applicationRequest = receivedApplicationMap[userId]{
            guard !applicationRequest.requestId.isEmpty else { return }
            try await internalCancelRequest(requestId: applicationRequest.requestId)
            self.removeSeatApplication(userId: userId)
        } else {
            guard let invitationRequest = sentInvitationMap[userId], !invitationRequest.requestId.isEmpty else { return }
            try await internalCancelRequest(requestId: invitationRequest.requestId)
            self.removeSentSeatInvitation(userId: userId)
        }
    }
    
    private func internalCancelRequest(requestId: String) async throws {
        do {
            try await self.service.cancelRequest(requestId: requestId)
        } catch let SeatGridViewError.error(code, message) {
            VRLog.error("\(#file)","\(#line)",
                                  "cancelRequest:[code:\(code),message:\(message)]")
            throw SeatGridViewError.error(code: code, message: message)
        } catch {
            assert(false, "unknown error, description:[:\(error.localizedDescription)]")
        }
    }
    
    private func removeSeatApplication(userId: String) {
        receivedApplicationMap.removeValue(forKey: userId)
    }
    
    private func addSentSeatApplication(request: TUIRequest) {
        if isRequestInvalid(request: request) {
            return
        }
        receivedApplicationMap[request.userId] = request
    }
    
    private func addSentSeatInvitation(request: TUIRequest) {
        if isRequestInvalid(request: request) {
            return
        }
        sentInvitationMap[request.userId] = request
    }
    
    private func removeSentSeatApplication(userId: String) {
        receivedApplicationMap.removeValue(forKey: userId)
    }
    
    private func removeSentSeatInvitation(userId: String) {
        sentInvitationMap.removeValue(forKey: userId)
    }
    
    private func removeReceivedSeatInvitation() {
        receivedInvitation = nil
    }
    
    private func isRequestInvalid(request: TUIRequest) -> Bool {
        if request.userId.isEmpty {
            return true
        }
        return request.requestId.isEmpty
    }
    
    private func isSeatRequest(request: TUIRequest) -> Bool {
        return request.requestAction == .takeSeat || request.requestAction == .remoteUserOnSeat
    }
    
    private func updateSeatInfo(seatInfoList: [TUISeatInfo]) {
        observerState.update { seatState in
            seatState.seatList = seatInfoList
        }
    }
}

// MARK: - Engine Observer Event.
extension SGSeatManager {
    func onRequestReceived(request: TUIRequest) {
        switch request.requestAction {
            case .takeSeat:
                onReceivedTakeSeat(request: request)
            case .remoteUserOnSeat:
                onReceivedInviteSeat(request: request)
            default:
                break
        }
    }
    
    func onRequestCancelled(request: TUIRequest, operateUser: TUIUserInfo) {
        switch request.requestAction {
            case .takeSeat:
                onTakeSeatCancelled(request: request)
            case .remoteUserOnSeat:
                onInviteSeatCancelled(request: request)
            default:
                break
        }
    }
    
    func onRequestProcessed(request: TUIRequest, operateUser: TUIUserInfo) {
        if !isSeatRequest(request: request) {
            return
        }
        if request.requestAction == .remoteUserOnSeat {
            removeSeatApplication(userId: request.userId)
        }
    }
    
    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        updateSeatInfo(seatInfoList: seatList)
    }
    
    private func onReceivedTakeSeat(request: TUIRequest) {
        guard !isRequestInvalid(request: request) else { return }
        receivedApplicationMap[request.userId] = request
    }
    
    private func onReceivedInviteSeat(request: TUIRequest) {
        guard !isRequestInvalid(request: request) else { return }
        receivedInvitation = request
    }
    
    private func onTakeSeatCancelled(request: TUIRequest) {
        removeSeatApplication(userId: request.userId)
    }
    
    private func onInviteSeatCancelled(request: TUIRequest) {
        removeReceivedSeatInvitation()
    }
}
