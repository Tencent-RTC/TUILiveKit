//
//  AnchorRoomState.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2024/11/18.
//

import RTCRoomEngine

struct AnchorRoomState {
    var roomId: String = ""
    var liveInfo: TUILiveInfo = TUILiveInfo()
    var createTime: UInt = 0
    var roomName: String = ""
    var coverURL: String = ""
    var userCount: Int = 0
    var liveStatus: LiveStatus = .none
    var liveExtraInfo: AnchorLiveExtraInfo = AnchorLiveExtraInfo()
    var pkTemplateId: Int = 0
    
    struct AnchorLiveExtraInfo: Codable {
        var liveMode: LiveStreamPrivacyStatus = .public
        var maxAudienceCount: Int = 0
        var messageCount: Int = 0
        var giftTotalCoins: Int = 0
        var giftTotalUniqueSender: Int = 0
        var likeTotalUniqueSender: Int = 0
        var activeStatus: Int = 0
    }
}
