//
//  LSRoomState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import RTCRoomEngine
import TUILiveResources

struct LSRoomState {
    var roomId: String = ""
    var roomInfo: TUIRoomInfo = TUIRoomInfo()
    var createTime: UInt = 0
    var roomName: String = ""
    var coverURL: String = ""
    var userCount: Int = 0
    var liveStatus: LiveStatus = .none
    var liveExtraInfo: LiveExtraInfo = LiveExtraInfo()
    
    struct LiveExtraInfo: Codable {
        var liveMode: LiveStreamPrivacyStatus = .public
        var maxAudienceCount: Int = 0
        var messageCount: Int = 0
        var giftIncome: Int = 0
        var giftPeopleSet: Set<String> = []
        var likeCount: Int = 0
        var activeStatus: Int = 0
    }
}
