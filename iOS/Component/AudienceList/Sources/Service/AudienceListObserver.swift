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
    private weak var service: AudienceListService?
    
    init(state: AudienceListState, service: AudienceListService) {
        self.state = state
        self.service = service
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        if userCount > 100 {
            state.audienceCount = userCount - 1
        }
    }
    
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        guard userInfo.userId != state.ownerId else { return }
        guard !state.audienceList.contains(where: { $0.userId == userInfo.userId }) else { return }
        if state.audienceList.count < kMaxShowUserCount {
            state.audienceList.append(userInfo)
        }
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        state.audienceList.removeAll(where: { $0.userId == userInfo.userId })
    }
    
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        state.roomDismissedSubject.send(roomId)
    }
}
