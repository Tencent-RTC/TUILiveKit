//
//  AudienceListState.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/10/31.
//

import Foundation
import RTCRoomEngine

public class AudienceListState {
    public var roomId: String = ""
    public var ownerId: String = ""
    @Published var audienceCount: Int = 0
    @Published var audienceList: [TUIUserInfo] = []
}

let kMaxShowUserCount = 100
