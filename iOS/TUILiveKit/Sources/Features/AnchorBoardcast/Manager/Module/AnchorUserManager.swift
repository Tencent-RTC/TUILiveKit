//
//  AnchorUserManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine
import LiveStreamCore

class AnchorUserManager {
    private let observerState = ObservableState<AnchorUserState>(initialState: AnchorUserState())
    var userState: AnchorUserState {
        observerState.state
    }
    
    private typealias Context = AnchorManager.Context
    private weak var context: Context?
    private let service: AnchorService
    
    private let volumnCanHearMinLimit = 25
    
    init(context: AnchorManager.Context) {
        self.context = context
        self.service = context.service
    }
}

// MARK: - Interface
extension AnchorUserManager {
    func onStartLive() {
        Task {
            do {
                let userList = try await service.getUserList()
                update { state in
                    state.userList.removeAll()
                    for userInfo in userList {
                        if userInfo.userId == context?.coreRoomState.ownerInfo.userId {
                            continue
                        }
                        state.userList.insert(userInfo)
                    }
                }
            } catch let err as InternalError {
                context?.toastSubject.send(err.localizedMessage)
            }
        }
    }
    
    func onLeaveLive() {
        update { state in
            state = AnchorUserState()
        }
    }
    
    func getUserInfo(userId: String) async throws -> TUIUserInfo {
        try await service.getUserInfo(userId: userId)
    }
    
    func onDisableSendingMessageBtnClicked(userId: String, isDisable: Bool) async throws {
        try await service.disableSendingMessageByAdmin(userId: userId, isDisable: isDisable)
    }
    
    func onKicedOutBtnClicked(userId: String) async throws {
        try await service.kickRemoteUserOutOfRoom(userId: userId)
    }
    
    func subscribeState<Value>(_ selector: StateSelector<AnchorUserState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Observer
extension AnchorUserManager {
    func onUserVoiceVolumeChanged(volumeMap: [String: NSNumber]) {
        for (userId, volume) in volumeMap {
            if volume.intValue > volumnCanHearMinLimit {
                update { state in
                    state.speakingUserList.insert(userId)
                }
            } else {
                update { state in
                    state.speakingUserList.remove(userId)
                }
            }
        }
    }
    
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        guard roomId == context?.roomManager.roomState.roomId else { return }
        if userInfo.userId == context?.coreRoomState.ownerInfo.userId {
            return
        }
        update { state in
            state.userList.insert(userInfo)
        }
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        guard roomId == context?.roomManager.roomState.roomId else { return }
        update { state in
            state.userList.remove(userInfo)
        }
    }
    
    func onUserInfoChanged(userInfo: TUIUserInfo, modifyFlag: TUIUserInfoModifyFlag) {
        update { state in
            if let user = state.userList.first(where: { $0.userId == userInfo.userId }) {
                if modifyFlag.contains(.userRole) {
                    user.userRole = userInfo.userRole
                }
            }
        }
    }
    
    func OnSendMessageForUserDisableChanged(roomId: String, userId: String, isDisable muted: Bool) {
        guard roomId == context?.coreRoomState.roomId, userId == context?.coreUserState.selfInfo.userId else { return }
        context?.toastSubject.send(muted ? .messageDisabledText : .messageEnabledText)
    }
}

extension AnchorUserManager {
    private func update(userState: AnchorUserStateUpdateClosure) {
        observerState.update(reduce: userState)
    }
}

fileprivate extension String {
    static let messageDisabledText = internalLocalized("You have been muted in the current room")
    static let messageEnabledText =  internalLocalized("You have been unmuted in the current room")
}
