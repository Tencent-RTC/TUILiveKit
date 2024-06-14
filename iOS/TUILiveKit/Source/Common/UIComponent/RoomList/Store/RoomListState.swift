//
//  RoomListState.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import Foundation
import RTCRoomEngine

struct RoomListResult {
    var cursor: String = ""
    var roomInfoList: [TUILiveInfo] = []
}

extension RoomListResult: Equatable {}

struct RoomListState {
    var roomInfoListResult: RoomListResult = RoomListResult()
}

struct RoomListNavigationState {
    enum Router {
        case exit
        case main
        case toLive(_ liveInfo: TUILiveInfo)
    }
    var currentRouter: Router = .main
}

extension RoomListNavigationState.Router: Equatable {
    static func ==(lhs: RoomListNavigationState.Router, rhs: RoomListNavigationState.Router) -> Bool {
        switch (lhs, rhs) {
        case (.exit, .exit),
            (.main, .main):
            return true
        case let (.toLive(l), .toLive(r)):
            return l == r
        case (.exit, _),
            (.main, _),
            (.toLive,_):
            return false
        }
    }
}
