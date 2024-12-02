//
//  LiveListState.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import Foundation
import RTCRoomEngine

struct LiveListResult {
    var cursor: String = ""
    var liveInfoList: [TUILiveInfo] = []
}

extension LiveListResult: Equatable {}

struct LiveListState {
    var liveInfoListResult: LiveListResult = LiveListResult()
}

struct LiveListNavigationState {
    enum Router {
        case exit
        case main
        case toLive(_ liveInfo: TUILiveInfo)
    }
    var currentRouter: Router = .main
}

extension LiveListNavigationState.Router: Equatable {
    static func ==(lhs: LiveListNavigationState.Router, rhs: LiveListNavigationState.Router) -> Bool {
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
