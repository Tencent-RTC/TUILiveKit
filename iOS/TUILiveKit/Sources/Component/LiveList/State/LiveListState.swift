//
//  LiveListState.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import Foundation
import RTCRoomEngine

public struct LiveInfo {

    var coverUrl = ""
    
    var backgroundUrl = ""
    
    var categoryList: [NSNumber] = []
    
    var isPublicVisible = true
    
    var activityStatus: Int = 0
    
    var viewCount: Int = 0
    
    var roomId = ""
    
    var ownerId = ""
    
    var ownerName = ""
    
    var ownerAvatarUrl = ""
    
    var name = "";
    
    init(tuiLiveInfo: TUILiveInfo) {
        self.coverUrl = tuiLiveInfo.coverUrl
        self.backgroundUrl = tuiLiveInfo.backgroundUrl
        self.categoryList = tuiLiveInfo.categoryList
        self.isPublicVisible = tuiLiveInfo.isPublicVisible
        self.activityStatus = tuiLiveInfo.activityStatus
        self.viewCount = tuiLiveInfo.viewCount
        self.roomId = tuiLiveInfo.roomInfo.roomId
        self.ownerId = tuiLiveInfo.roomInfo.ownerId
        self.ownerName = tuiLiveInfo.roomInfo.ownerName
        self.ownerAvatarUrl = tuiLiveInfo.roomInfo.ownerAvatarUrl
        self.name = tuiLiveInfo.roomInfo.name
    }
    
    init() {}
}

extension LiveInfo: Equatable {}

struct LiveListResult {
    var isFirstFetch: Bool = true
    var cursor: String = ""
    var liveInfoList: [LiveInfo] = []
}

extension LiveListResult: Equatable {}

struct LiveListState {
    var liveInfoListResult: LiveListResult = LiveListResult()
}

struct LiveListNavigationState {
    enum Router {
        case main
        case toLive(_ liveInfo: LiveInfo)
    }
    var currentRouter: Router = .main
}

extension LiveListNavigationState.Router: Equatable {
    static func ==(lhs: LiveListNavigationState.Router, rhs: LiveListNavigationState.Router) -> Bool {
        switch (lhs, rhs) {
        case (.main, .main):
            return true
        case let (.toLive(l), .toLive(r)):
            return l == r
        case (.main, _),
            (.toLive,_):
            return false
        }
    }
}
