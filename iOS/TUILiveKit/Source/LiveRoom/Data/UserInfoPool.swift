//
//  UserInfoPool.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/10.
//

import Foundation
import TUICore

class UserInfoPool {
    private let roomId: String
    init(roomId: String) {
        self.roomId = roomId
    }
    
    @WeakLazyInjected var engineManager:EngineServiceProvider?
    private var engineService: RoomEngineService? {
        return engineManager?.getRoomEngineService(roomId: roomId)
    }
    
    private var pool: [String: UserInfo] = [:]
    
    func get(_ userId: String) -> UserInfo {
        guard let userInfo = pool[userId] else {
            var userInfo: UserInfo
            if TUILogin.getUserID() ?? "" == userId , let engineService = engineService {
                userInfo = engineService.liveRoomInfo.selfInfo
            } else {
                userInfo = UserInfo()
                userInfo.userId = userId
            }
            pool[userId] = userInfo
            return userInfo
        }
        return userInfo
    }

    func isExist(_ userId: String)-> Bool {
        return (pool[userId] != nil) ? true: false
    }
    
    func remove(_ userId: String) {
        pool.removeValue(forKey: userId)
    }

    func clearAll() {
        pool = [:]
    }
}
