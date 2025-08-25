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

    public func initLiveInfo(liveInfo: TUILiveInfo) {
        state.roomId = liveInfo.roomId
        state.ownerId = liveInfo.ownerId
        self.getUserList()
    }
    
    func getUserList() {
        TUIRoomEngine.sharedInstance().getUserList(nextSequence: 0) { [weak self] userInfoList, cursor in
            guard let self = self else { return }
            if !userInfoList.isEmpty {
                var list = userInfoList.prefix(kMaxShowUserCount + 1)
                list = list.filter{ $0.userId != self.state.ownerId }
                if list.count > kMaxShowUserCount {
                    list.removeLast()
                }
                self.state.audienceList = Array(list)
            }
        } onError: { code, message in
            
        }
    }
}
