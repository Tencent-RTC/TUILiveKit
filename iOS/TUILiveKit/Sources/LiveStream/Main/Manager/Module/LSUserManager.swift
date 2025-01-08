//
//  LSUserManager.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/19.
//

import Foundation
import RTCCommon
import RTCRoomEngine
import Combine

class LSUserManager {
    private let observerState = ObservableState<LSUserState>(initialState: LSUserState())
    var userState: LSUserState {
        observerState.state
    }
    
    private typealias Context = LiveStreamManager.Context
    private weak var context: Context?
    private let toastSubject: PassthroughSubject<String, Never>
    private let service: LSRoomEngineService
    
    private let volumnCanHearMinLimit = 25
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        self.service = context.service
        self.toastSubject = context.toastSubject
        initSelfUserData()
    }
    
    func resetState() {
        update { state in
            state = LSUserState()
        }
    }
}

extension LSUserManager {
    func subscribeState<Value>(_ selector: StateSelector<LSUserState, Value>) -> AnyPublisher<Value, Never> {
        return observerState.subscribe(selector)
    }
}

// MARK: - Interface
extension LSUserManager {
    func fetchAudienceList() {
        Task {
            do {
                let userList = try await service.getUserList()
                update { state in
                    state.userList.removeAll()
                    for userInfo in userList {
                        if userInfo.userId == context?.roomManager.roomState.ownerInfo.userId {
                            continue
                        }
                        let liveUser = LSUser(userInfo: userInfo)
                        state.userList.insert(liveUser)
                    }
                }
            } catch let err {
                toastSubject.send(err.localizedDescription)
            }
        }
    }
    
    func muteAllRemoteAudio(isMute: Bool) {
        service.muteAllRemoteAudio(isMute: isMute)
    }
    
    func updateSelfUserInfo() {
        Task {
            do {
                let userInfo = try await service.getUserInfo(userId: userState.selfInfo.userId)
                update { state in
                    state.selfInfo.role = userInfo.userRole
                }
            } catch let err {
                toastSubject.send(err.localizedDescription)
            }
        }
    }
    
    func updateOwnerUserInfo() {
        guard let ownerId = context?.roomManager.roomState.ownerInfo.userId, !ownerId.isEmpty else { return }
        if ownerId == userState.selfInfo.userId {
            update { state in
                state.selfInfo.role = .roomOwner
            }
        }
        Task {
            do {
                let ownerInfo = try await service.getUserInfo(userId: ownerId)
                context?.roomManager.update { roomState in
                    roomState.ownerInfo = LSUser(userInfo: ownerInfo)
                }
            } catch let err {
                toastSubject.send(err.localizedDescription)
            }
        }
    }
    
    func update(userState: LSUserStateUpdateClosure) {
        observerState.update(reduce: userState)
    }
}

// MARK: - Observer
extension LSUserManager {
    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason) {
        if hasAudio {
            update { state in
                state.hasAudioStreamUserList.insert(userId)
            }
        } else {
            update { state in
                state.hasAudioStreamUserList.remove(userId)
            }
        }
    }
    
    func onUserVideoStateChanged(userId: String, streamType: TUIVideoStreamType, hasVideo: Bool, reason: TUIChangeReason) {
        if hasVideo {
            update { state in
                state.hasVideoStreamUserList.insert(userId)
            }
        } else {
            update { state in
                state.hasVideoStreamUserList.remove(userId)
            }
        }
    }
    
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
        if userInfo.userId == context?.roomManager.roomState.ownerInfo.userId {
            return
        }
        let user = LSUser(userInfo: userInfo)
        update { state in
            state.enterUserInfo = user
            state.userList.insert(user)
        }
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        let user = LSUser(userInfo: userInfo)
        update { state in
            state.userList.remove(user)
        }
    }
    
    func onUserInfoChanged(userInfo: TUIUserInfo, modifyFlag: TUIUserInfoModifyFlag) {
        update { state in
            let userList = state.userList
            if var user = userList.first(where: { $0.userId == userInfo.userId }) {
                if modifyFlag.contains(.userRole) {
                    user.role = userInfo.userRole
                }
                state.userList.remove(user)
                state.userList.insert(user)
            } else if state.selfInfo.userId == userInfo.userId {
                if modifyFlag.contains(.userRole) {
                    state.selfInfo.role = userInfo.userRole
                }
            }
        }
    }
}

// MARK: - Private
extension LSUserManager {
    private func initSelfUserData() {
        let loginUserInfo = service.getSelfInfo()
        update { state in
            state.selfInfo.userId = loginUserInfo.userId
            state.selfInfo.name = loginUserInfo.userName
            state.selfInfo.avatarUrl = loginUserInfo.avatarUrl
        }
    }
}
