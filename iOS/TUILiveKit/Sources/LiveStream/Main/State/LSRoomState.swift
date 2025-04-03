//
//  LSRoomState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import RTCRoomEngine

struct LSRoomState {
    var roomId: String = ""
    var createTime: UInt = 0
    var roomName: String = ""
    var coverURL: String = Constants.URL.defaultCover
    var userCount: Int = 0
    var liveStatus: LiveStatus = .none
    var liveExtraInfo: LiveExtraInfo = LiveExtraInfo()
    
    struct LiveExtraInfo: Codable {
        var category: LiveStreamCategory = .chat
        var liveMode: LiveStreamPrivacyStatus = .public
        var maxAudienceCount: Int = 0
        var messageCount: Int = 0
        var giftIncome: Int = 0
        var giftPeopleSet: Set<String> = []
        var likeCount: Int = 0
        var activeStatus: Int = 0
    }
}
