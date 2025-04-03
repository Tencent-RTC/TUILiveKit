//
//  RoomInfoState.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/10/31.
//

import Foundation
import RTCRoomEngine
import Combine

class RoomInfoState {
    var selfUserId: String = ""
    var roomId: String = ""
    @Published var ownerId: String = ""
    @Published var ownerName: String = ""
    @Published var ownerAvatarUrl: String = ""
    @Published var fansNumber: Int = 0
    @Published var followingList: Set<TUIUserInfo> = []
    let roomDismissedSubject = PassthroughSubject<String, Never>()
}
