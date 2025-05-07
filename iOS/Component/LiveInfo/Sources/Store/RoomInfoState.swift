//
//  RoomInfoState.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/10/31.
//

import Foundation
import RTCRoomEngine
import Combine

public class RoomInfoState {
    public var selfUserId: String = ""
    public var roomId: String = ""
    @Published public var ownerId: String = ""
    @Published public var ownerName: String = ""
    @Published public var ownerAvatarUrl: String = ""
    @Published public var fansNumber: Int = 0
    @Published public var followingList: Set<TUIUserInfo> = []
    public let roomDismissedSubject = PassthroughSubject<String, Never>()
}
