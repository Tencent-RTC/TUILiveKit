//
//  LiveListViewDefine.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/4/29.
//

import RTCRoomEngine
 
public enum LiveListViewStyle {
    case singleColumn
    case doubleColumn
}

public protocol LiveListViewAdapter: AnyObject {
    func createLiveInfoView(info: LiveInfo) -> UIView
    func updateLiveInfoView(view: UIView, info: LiveInfo) -> Void
}

public protocol LiveListDataSource: AnyObject {
    typealias LiveListBlock = (String, [LiveInfo]) -> Void
    func fetchLiveList(cursor: String, onSuccess: @escaping LiveListBlock, onError: @escaping TUIErrorBlock)
}

public protocol OnItemClickDelegate: AnyObject {
    func onItemClick(liveInfo: LiveInfo, frame: CGRect)
}

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
