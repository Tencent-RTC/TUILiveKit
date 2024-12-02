//
//  AudienceListObserver.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/10/31.
//

import Foundation
import RTCRoomEngine

class AudienceListObserver: NSObject, TUIRoomObserver {
    private let state: AudienceListState
    
    init(state: AudienceListState) {
        self.state = state
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        if userCount > 100 {
            state.audienceCount = userCount - 1
        }
    }
    
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        // TODO: How to deal with a large number of users
        guard userInfo.userId != state.ownerId else { return }
        if !state.audienceList.contains(where: { $0.userId == userInfo.userId }) {
            state.audienceList.append(userInfo)
        }
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        state.audienceList.removeAll(where: { $0.userId == userInfo.userId })
    }
}
