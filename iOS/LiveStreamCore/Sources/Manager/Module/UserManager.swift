//
//  UserManager.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/24.
//

import RTCCommon
import RTCRoomEngine

class UserManager {
    let observerState = ObservableState<UserState>(initialState: UserState())
    var userState: UserState {
        observerState.state
    }
    
    private weak var context: LiveStreamManager.Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        initSelfUserData()
    }
    
    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason) {
        observerState.update(isPublished: false) { userState in
            if hasAudio {
                userState.hasAudioStreamUserList.insert(userId)
            } else {
                userState.hasAudioStreamUserList.remove(userId)
            }
        }
        if userId == userState.selfInfo.userId {
            context?.mediaManager.onSelfAudioStateChanged(hasAudio: hasAudio)
        }
    }
    
    func onUserVideoStateChanged(userId: String, hasVideo: Bool, reason: TUIChangeReason) {
        guard let context = context else { return }
        
        if hasVideo {
            observerState.update { userState in
                userState.hasVideoStreamUserList.insert(userId)
            }
            if !isSelf(userId: userId) {
                Task {
                    let _ = try await context.mediaManager.startPlayRemoteVideo(userId: userId, streamType: .cameraStream)
                }
            }
        } else {
            observerState.update { userState in
                userState.hasVideoStreamUserList.remove(userId)
            }
            if !isSelf(userId: userId) {
                context.mediaManager.stopPlayRemoteVideo(userId: userId, streamType: .cameraStream)
            }
        }
        
        if isSelf(userId: userId) {
            context.mediaManager.onSelfVideoStateChanged(hasVideo: hasVideo)
        }
    }
}

// MARK: - Callback from other manager
extension UserManager {
    func onStartLiveSuccess() {
        observerState.update(isPublished: false) { userState in
            userState.selfInfo.userRole = .roomOwner
        }
    }
    
    func onLeaveRoom() {
        observerState.update(isPublished: false) { userState in
            userState.selfInfo.userRole = .generalUser
            userState.hasAudioStreamUserList.removeAll()
            userState.hasVideoStreamUserList.removeAll()
        }
    }
}

// MARK: - Private
extension UserManager {
    private func initSelfUserData() {
        let loginUserInfo = TUIRoomEngine.getSelfInfo()
        observerState.update(isPublished: false) { userState in
            userState.selfInfo.userId = loginUserInfo.userId
            userState.selfInfo.userName = loginUserInfo.userName
            userState.selfInfo.avatarUrl = loginUserInfo.avatarUrl
        }
    }
    
    private func isSelf(userId: String) -> Bool {
        return userId == userState.selfInfo.userId
    }
}
