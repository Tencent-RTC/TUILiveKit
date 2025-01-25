//
//  UserState.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import RTCRoomEngine

public struct UserState: State {
    public var selfInfo: TUIUserInfo = TUIUserInfo()
    public var hasAudioStreamUserList: Set<String> = []
    public var hasVideoStreamUserList: Set<String> = []
    
    public init() {}
}
