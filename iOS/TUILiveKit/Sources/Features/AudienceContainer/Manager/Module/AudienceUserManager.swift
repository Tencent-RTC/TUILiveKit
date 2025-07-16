//
//  AudienceUserManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine
import LiveStreamCore

class AudienceUserManager {
    private let observerState = ObservableState<AudienceUserState>(initialState: AudienceUserState())
    var userState: AudienceUserState {
        observerState.state
    }
    
    private typealias Context = AudienceManager.Context
    private weak var context: Context?
    private let service: AudienceService
    
    private let volumnCanHearMinLimit = 25
    
    init(context: AudienceManager.Context) {
        self.context = context
        self.service = context.service
    }
}

// MARK: - Interface
extension AudienceUserManager {
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
            state = AudienceUserState()
        }
    }
    
    func getUserInfo(userId: String) async throws -> TUIUserInfo {
        try await service.getUserInfo(userId: userId)
    }
    
    func subscribeState<Value>(_ selector: StateSelector<AudienceUserState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Observer
extension AudienceUserManager {
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

extension AudienceUserManager {
    private func update(userState: AudienceUserStateUpdateClosure) {
        observerState.update(reduce: userState)
    }
}

fileprivate extension String {
    static let messageDisabledText = internalLocalized("You have been muted in the current room")
    static let messageEnabledText =  internalLocalized("You have been unmuted in the current room")
}
