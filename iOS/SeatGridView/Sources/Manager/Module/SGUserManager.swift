//
//  SGUserManager.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/23.
//  Updated by abyyxwang on 2024/11/7.
//

import RTCRoomEngine
import RTCCommon

class SGUserManager {
    let observerState = ObservableState(initialState: SGUserState())
    var userState: SGUserState {
        observerState.state
    }
    
    private typealias Context = SeatGridViewManager.Context
    private weak var context: Context?
    private let service: SeatGridViewInterface
    
    init(context: SeatGridViewManager.Context) {
        self.context = context
        self.service = context.service
    }
    
    func refreshSelfInfo() {
        let selfInfo = self.service.getSelfInfo()
        update(login: selfInfo)
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

// MARK: RoomEngine event hook.
extension SGUserManager {
    func onUserVoiceVolumeChanged(volumeMap: [String: Int]) {
        let result = volumeMap
            .flatMap { key, value in
                return value > 25 ? [SGUserVolume(userId: key, volume: value)] : []
            }
        let speakUsers = Set(result)
        observerState.update { userState in
            userState.speakingUserList = speakUsers
        }
    }
    
    func onUserAudioStateChanged(userId: String, hasAudio: Bool) {
        observerState.update { userState in
            if hasAudio {
                userState.hasAudioStreamUserList.insert(userId)
            } else {
                userState.hasAudioStreamUserList.remove(userId)
            }
        }
    }
}

extension SGUserManager {
    private func update(login userInfo: TUILoginUserInfo) {
        userState.selfInfo.userId = userInfo.userId
        userState.selfInfo.userName = userInfo.userName
        userState.selfInfo.avatarUrl = userInfo.avatarUrl
    }
}



