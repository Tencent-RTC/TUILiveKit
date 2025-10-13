//
//  SGTypeConvert.swift
//  TUILiveKit
//
//  Created by CY zhao on 2025/9/3.
//

import Foundation
import AtomicXCore
import RTCRoomEngine

typealias AtomicLiveInfo = AtomicXCore.LiveInfo

extension AtomicLiveInfo {
    init(from tuiLiveInfo: TUILiveInfo) {
        self.init()
        liveID = tuiLiveInfo.roomId
        liveName = tuiLiveInfo.name
        notice = tuiLiveInfo.notice
        isMessageDisable = tuiLiveInfo.isMessageDisableForAllUser
        isPublicVisible = tuiLiveInfo.isPublicVisible
        isSeatEnabled = tuiLiveInfo.isSeatEnabled
        keepOwnerOnSeat = tuiLiveInfo.keepOwnerOnSeat
        maxSeatCount = tuiLiveInfo.maxSeatCount
        seatMode = tuiLiveInfo.seatMode == .applyToTake ? .apply : .free
        seatLayoutTemplateID = tuiLiveInfo.seatLayoutTemplateId
        coverURL = tuiLiveInfo.coverUrl
        backgroundURL = tuiLiveInfo.backgroundUrl
        categoryList = tuiLiveInfo.categoryList
        activityStatus = tuiLiveInfo.activityStatus
    }
}

extension TUILiveInfo {
    convenience init(from liveInfo: AtomicLiveInfo) {
        self.init()
        roomId = liveInfo.liveID
        name = liveInfo.liveName
        notice = liveInfo.notice
        isMessageDisableForAllUser = liveInfo.isMessageDisable
        isPublicVisible = liveInfo.isPublicVisible
        isSeatEnabled = liveInfo.isSeatEnabled
        keepOwnerOnSeat = liveInfo.keepOwnerOnSeat
        maxSeatCount = liveInfo.maxSeatCount
        seatMode = TUISeatMode(from: liveInfo.seatMode)
        seatLayoutTemplateId = liveInfo.seatLayoutTemplateID
        coverUrl = liveInfo.coverURL
        backgroundUrl = liveInfo.backgroundURL
        categoryList = liveInfo.categoryList
        activityStatus = liveInfo.activityStatus
        safeSetPropertyIfSetterExists(liveInfo.liveOwner.userID, forKey: "ownerId")
        safeSetPropertyIfSetterExists(liveInfo.liveOwner.userName, forKey: "ownerName")
        safeSetPropertyIfSetterExists(liveInfo.liveOwner.avatarURL, forKey: "ownerAvatarUrl")
        safeSetPropertyIfSetterExists(liveInfo.createTime, forKey: "createTime")
        viewCount = liveInfo.totalViewerCount
    }
}

extension TUISeatMode {
    init(from takeSeatMode: TakeSeatMode) {
        switch takeSeatMode {
        case .apply:
            self = .applyToTake
        default:
            self = .freeToTake
        }
    }
}

extension TakeSeatMode {
    init(from tuiSeatMode: TUISeatMode) {
        switch tuiSeatMode {
        case .applyToTake:
            self = .apply
        default:
            self = .free
        }
    }
}
    
extension TUIUserInfo {
    convenience init(from liveUserInfo: LiveUserInfo) {
        self.init()
        userId = liveUserInfo.userID
        userName = liveUserInfo.userName
        avatarUrl = liveUserInfo.avatarURL
    }
    
    convenience init(userId: String) {
        self.init()
        self.userId = userId
    }
}

extension TUISeatInfo {
    convenience init(from seatInfo: SeatInfo) {
        self.init()
        index = seatInfo.index
        userId = seatInfo.userInfo.userID
        userName = seatInfo.userInfo.userName
        avatarUrl = seatInfo.userInfo.avatarURL
        isLocked = seatInfo.isLocked
        isVideoLocked = !seatInfo.userInfo.allowOpenCamera
        isAudioLocked = !seatInfo.userInfo.allowOpenMicrophone
    }
}

extension TUIKickedOutOfRoomReason {
    init(from liveKickoutReason: LiveKickedOutReason) {
        switch liveKickoutReason {
        case .byAdmin:
            self = .byAdmin
        case .byLoggedOnOtherDevice:
            self = .byLoggedOnOtherDevice
        case .byServer:
            self = .byServer
        case .forNetworkDisconnected:
            self = .forNetworkDisconnected
        case .forJoinRoomStatusInvalidDuringOffline:
            self = .forJoinRoomStatusInvalidDuringOffline
        case .forCountOfJoinedRoomsExceedLimit:
            self = .forCountOfJoinedRoomsExceedLimit
        }
    }
}
