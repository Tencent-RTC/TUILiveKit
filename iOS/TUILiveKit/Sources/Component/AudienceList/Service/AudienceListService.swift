//
//  AudienceListService.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/10/31.
//

import Foundation
import RTCRoomEngine

class AudienceListService {
    public let state = AudienceListState()
    
    public func initRoomInfo(roomId: String) {
        state.roomId = roomId
        TUIRoomEngine.sharedInstance().fetchRoomInfo(roomId: roomId, roomType: .live) { [weak self] roomInfo in
            guard let self = self, let roomInfo = roomInfo else { return }
            self.state.ownerId = roomInfo.ownerId
            self.getUserList()
        } onError: { code, message in
            
        }
    }
    
    private func getUserList() {
        TUIRoomEngine.sharedInstance().getUserList(nextSequence: 0) { [weak self] userInfoList, cursor in
            guard let self = self else { return }
            if !userInfoList.isEmpty {
                self.state.audienceList = userInfoList.filter{ $0.userId != self.state.ownerId }
            }
        } onError: { code, message in
            
        }
    }
}
