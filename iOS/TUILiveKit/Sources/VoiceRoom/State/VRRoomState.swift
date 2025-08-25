//
//  VRRoomState.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/7.
//

// PassthroughSubject
// CurrentValueSubject

import Foundation
import RTCRoomEngine

public enum RoomPrivacyMode: NSInteger, CaseIterable {
    case `public` = 0
    case privacy = 1
}

struct VRRoomState {
    var roomId: String = ""
    var liveInfo: TUILiveInfo = TUILiveInfo()
    var createTime: UInt = 0
    var roomName: String = ""
    var coverURL: String = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png"
    var backgroundURL: String = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background1.png"    
    var userCount: Int = 0
    var liveExtraInfo: LiveExtraInfo = LiveExtraInfo()
    
    struct LiveExtraInfo: Codable {
        var category: LiveStreamCategory = .chat
        var liveMode: LiveStreamPrivacyStatus = .public
        var maxAudienceCount: Int = 0
        var messageCount: Int = 0
        var giftTotalCoins: Int = 0
        var giftTotalUniqueSender: Int = 0
        var likeTotalUniqueSender: Int = 0
    }
}

extension VRRoomState: Identifiable {
    var id: String {
        self.roomId
    }
}
