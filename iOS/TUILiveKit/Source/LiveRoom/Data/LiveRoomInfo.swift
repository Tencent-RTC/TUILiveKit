//
//  LiveRoomInfo.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/7.
//

import Foundation

class LiveRoomInfo {
    let roomId: Observable<String> = Observable("")
    let name: Observable<String> = Observable("")
    let coverUrl: Observable<String> = Observable("")
    let category: Observable<LiveStreamCategory> = Observable(.chat)
    let liveMode: Observable<LiveMode> = Observable(.public)
    let interactionType: Observable<InteractionType> = Observable(.broadcast)
    let anchorInfo: Observable<UserInfo> = Observable(UserInfo())
    let audienceList: Observable<[UserInfo]> = Observable([])
    let audienceCount: Observable<Int> = Observable(0)
    let linkingAudienceList: Observable<[UserInfo]> = Observable([])
    let roomConfig: RoomConfig = RoomConfig()
    let userLiveStatus: Observable<UserLiveStatus> = Observable(.none)
    var maxSeatCount:Int = 0
    var giftIncome:Int = 0
    var giftPeopleMap:[String:String] = [:]
    var createTime:TimeInterval = 0
    lazy var userInfoPool : UserInfoPool = {
        return UserInfoPool(roomId: roomId.value)
    }()
    
    init(roomId: String) {
        self.roomId.value = roomId
    }
    
    init() {
    }
}
