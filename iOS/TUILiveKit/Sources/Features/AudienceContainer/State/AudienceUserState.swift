//
//  AudienceUserState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import RTCRoomEngine
import LiveStreamCore

struct AudienceUserState {
    var userList: Set<TUIUserInfo> = []
    var speakingUserList: Set<String> = []
    var myFollowingUserList: Set<TUIUserInfo> = []
}

extension TUIUserInfo {
    static func == (lhs: TUIUserInfo, rhs: TUIUserInfo) -> Bool {
        return  lhs.userId == rhs.userId
    }
    
    open override var hash: Int {
        var hasher = Hasher()
        hasher.combine(userId)
        return hasher.finalize()
    }
    
    open override func isEqual(_ object: Any?) -> Bool {
        guard let other = object as? TUIUserInfo else { return false }
        return self.userId == other.userId
    }
    
    convenience init(coHostUser: CoHostUser) {
        self.init()
        self.userId = coHostUser.connectionUser.userId
        self.userName = coHostUser.connectionUser.userName
        self.avatarUrl = coHostUser.connectionUser.avatarUrl
        self.hasAudioStream = coHostUser.hasAudioStream
        self.hasVideoStream = coHostUser.hasVideoStream
    }
}
