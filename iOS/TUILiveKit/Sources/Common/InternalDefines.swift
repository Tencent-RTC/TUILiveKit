//
//  InternalDefines.swift
//  LiveStreamCore
//
//  Created by gg on 2025/2/25.
//

import RTCRoomEngine

extension Array: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == [TUISeatInfo] {
    func toString() -> String {
        var str = ""
        for info in base {
            str.append("\(info.coreExt.toString()), ")
        }
        return "[\(str.dropLast(2))]"
    }
}

extension LiveStreamCoreExtension where Base == [TUIConnectionUser] {
    func toString() -> String {
        var str = ""
        for user in base {
            str.append("\(user.coreExt.toString()), ")
        }
        return "[\(str.dropLast(2))]"
    }
}

extension LiveStreamCoreExtension where Base == [TUIBattleUser] {
    func toString() -> String {
        var str = ""
        for user in base {
            str.append("\(user.coreExt.toString()), ")
        }
        return "[\(str.dropLast(2))]"
    }
}

extension LiveStreamCoreExtension where Base == [String] {
    func toString() -> String {
        var str = ""
        for item in base {
            str.append("\(item), ")
        }
        return "[\(str.dropLast(2))]"
    }
}

extension TUISeatInfo: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUISeatInfo {
    func toString() -> String {
        var str = ""
        str.append("index:\(base.index)")
        str.append(", userId:\(base.userId ?? "null")")
        str.append(", isLocked:\(base.isLocked)")
        str.append(", isVideoLocked:\(base.isVideoLocked)")
        str.append(", isAudioLocked:\(base.isAudioLocked)")
        return "{\(str)}"
    }
}

extension TUIConnectionUser: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIConnectionUser {
    func toString() -> String {
        var str = ""
        str.append("roomId:\(base.roomId)")
        str.append(", userId:\(base.userId)")
        str.append(", joinConnectionTime:\(base.joinConnectionTime)")
        return "{\(str)}"
    }
}

extension TUIRequest: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIRequest {
    func toString() -> String {
        var str = ""
        str.append("requestId:\(base.requestId)")
        str.append(", requestAction:\(base.requestAction.coreExt.toString())")
        str.append(", userId:\(base.userId)")
        str.append(", timestamp:\(base.timestamp)")
        str.append(", content:\(base.content)")
        return "{\(str)}"
    }
}

extension TUIUserInfo: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIUserInfo {
    func toString() -> String {
        var str = ""
        str.append("userId:\(base.userId)")
        str.append(", userRole:\(base.userRole.coreExt.toString())")
        str.append(", hasAudioStream:\(base.hasAudioStream)")
        str.append(", hasVideoStream:\(base.hasVideoStream)")
        str.append(", hasScreenStream:\(base.hasScreenStream)")
        str.append(", isMessageDisabled:\(base.isMessageDisabled)")
        return "{\(str)}"
    }
}

extension TUIRoomInfo: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIRoomInfo {
    func toString() -> String {
        var str = ""
        str.append("rooId: \(base.roomId)")
        str.append(", ownerId: \(base.ownerId)")
        str.append(", roomType: \(base.roomType.coreExt.toString())")
        str.append(", roomName: \(base.name)")
        str.append(", isCameraDisableForAllUser: \(base.isCameraDisableForAllUser)")
        str.append(", isMicrophoneDisableForAllUser: \(base.isMicrophoneDisableForAllUser)")
        str.append(", isScreenShareDisableForAllUser: \(base.isScreenShareDisableForAllUser)")
        str.append(", isMessageDisableForAllUser: \(base.isMessageDisableForAllUser)")
        str.append(", isSeatEnabled: \(base.isSeatEnabled)")
        str.append(", seatMode: \(base.seatMode.coreExt.toString())")
        str.append(", maxSeatCount: \(base.maxSeatCount)")
        str.append(", memberCount: \(base.memberCount)")
        return str
    }
}

extension TUILiveInfo: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUILiveInfo {
    func toString() -> String {
        var str = ""
        str.append("rooId: \(base.roomId)")
        str.append(", ownerId: \(base.ownerId)")
        str.append(", roomName: \(base.name)")
        str.append(", isMessageDisableForAllUser: \(base.isMessageDisableForAllUser)")
        str.append(", isSeatEnabled: \(base.isSeatEnabled)")
        str.append(", seatMode: \(base.seatMode.coreExt.toString())")
        str.append(", maxSeatCount: \(base.maxSeatCount)")
        str.append(", keepOwnerOnSeat: \(base.keepOwnerOnSeat)")
        str.append(", activityStatus: \(base.activityStatus)")
        str.append(", isPublicVisible: \(base.isPublicVisible)")
        str.append(", categoryList: \(base.categoryList)")
        str.append(", notice: \(base.notice)")
        return str
    }
}

extension TUIBattleUser: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIBattleUser {
    func toString() -> String {
        var str = ""
        str.append("roomId: \(base.roomId)")
        str.append(", userId: \(base.userId)")
        str.append(", userName: \(base.userName)")
        str.append(", score: \(base.score)")
        return str
    }
}

extension TUIBattleConfig: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIBattleConfig {
    func toString() -> String {
        var str = ""
        str.append("duration: \(base.duration)")
        str.append(", needResponse: \(base.needResponse)")
        str.append(", extensionInfo: \(base.extensionInfo)")
        return str
    }
}

extension TUIBattleInfo: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIBattleInfo {
    func toString() -> String {
        var str = ""
        str.append("battleId: \(base.battleId)")
        str.append(", config: \(base.config.coreExt.toString())")
        str.append(", inviter: \(base.inviter.coreExt.toString())")
        str.append(", inviteeList: \(base.inviteeList.coreExt.toString())")
        str.append(", startTime: \(base.startTime)")
        str.append(", endTime: \(base.endTime)")
        return str
    }
}

extension TUIRoomType: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIRoomType {
    func toString() -> String {
        switch base {
            case .conference:
                return "conference"
            case .live:
                return "live"
            default:
                return "unknow:\(base.rawValue)"
        }
    }
}

extension TUISeatMode: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUISeatMode {
    func toString() -> String {
        switch base {
            case .applyToTake:
                return "applyToTake"
            case .freeToTake:
                return "freeToTake"
            default:
                return "unknow:\(base.rawValue)"
        }
    }
}

extension TUIRoomDismissedReason: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIRoomDismissedReason {
    func toString() -> String {
        switch base {
            case .byOwner:
                return "byOwner"
            case .byServer:
                return "byServer"
            default:
                return "unknow:\(base.rawValue)"
        }
    }
}

extension TUIKickedOutOfRoomReason: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIKickedOutOfRoomReason {
    func toString() -> String {
        switch base {
            case .byAdmin:
                return "byAdmin"
            case .byLoggedOnOtherDevice:
                return "byLoggedOnOtherDevice"
            case .byServer:
                return "byServer"
            default:
                return "unknow:\(base.rawValue)"
        }
    }
}

extension TUIChangeReason: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIChangeReason {
    func toString() -> String {
        switch base {
            case .byAdmin:
                return "byAdmin"
            case .bySelf:
                return "bySelf"
            default:
                return "unknow:\(base.rawValue)"
        }
    }
}

extension TUIVideoStreamType: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIVideoStreamType {
    func toString() -> String {
        switch base {
            case .cameraStream:
                return "camera"
            case .cameraStreamLow:
                return "cameraLow"
            case .screenStream:
                return "screen"
            default:
                return "unknow:\(base.rawValue)"
        }
    }
}

extension TUIRole: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIRole {
    func toString() -> String {
        switch base {
            case .roomOwner:
                return "owner"
            case .administrator:
                return "admin"
            case .generalUser:
                return "general"
            default:
                return "unknow:\(base.rawValue)"
        }
    }
}

extension TUIRequestAction: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUIRequestAction {
    func toString() -> String {
        switch base {
            case .invalidAction:
                return "invalidAction"
            case .openRemoteCamera:
                return "openRemoteCamera"
            case .openRemoteMicrophone:
                return "openRemoteMicrophone"
            case .takeSeat:
                return "takeSeat"
            case .remoteUserOnSeat:
                return "remoteUserOnSeat"
            case .applyToAdminToOpenLocalCamera:
                return "applyOpenLocalCamera"
            case .applyToAdminToOpenLocalMicrophone:
                return "applyOpenLocalMicrophone"
            case .applyToAdminToOpenLocalScreenShare:
                return "applyOpenLocalScreenShare"
            default:
                return "unknow:\(base.rawValue)"
        }
    }
}

extension TUISeatLayout: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUISeatLayout {
    func toString() -> String {
        var str = ""
        str.append("templateId: \(base.templateId)")
        str.append(", canvasWidth: \(base.canvasWidth)")
        str.append(", canvasHeight: \(base.canvasHeight)")
        str.append(", seatList: (\(base.seatList.count)) \(base.seatList.coreExt.toString())")
        return str
    }
}

extension LiveStreamCoreExtension where Base == [TUISeatFullInfo] {
    func toString() -> String {
        var str = ""
        for region in base {
            str.append("{\(region.coreExt.toString())}, ")
        }
        return "[\(str.dropLast(2))]"
    }
}

extension TUISeatFullInfo: LiveStreamCoreWrapper {}
extension LiveStreamCoreExtension where Base == TUISeatFullInfo {
    func toString() -> String {
        var str = ""
        str.append("roomId: \(base.roomId)")
        str.append(", seatIndex: \(base.seatIndex)")
        str.append(", isSeatLocked: \(base.isSeatLocked)")
        str.append(", userId: \(base.userId ?? "")")
        str.append(", userName: \(base.userName ?? "")")
        str.append(", userAvatar: \(base.userAvatar ?? "")")
        str.append(", micOpen: \(base.userMicrophoneStatus.rawValue)")
        str.append(", cameraOpen: \(base.userCameraStatus.rawValue)")
        str.append(", userSuspendStatus: \(base.userSuspendStatus.rawValue)")
        str.append(", x: \(base.x)")
        str.append(", y: \(base.y)")
        str.append(", width: \(base.width)")
        str.append(", height: \(base.height)")
        str.append(", zorder: \(base.zorder)")
        return str
    }
    
    func isEqual(other: TUISeatFullInfo) -> Bool {
        return base.roomId == other.roomId
        && base.seatIndex == other.seatIndex
        && base.isSeatLocked == other.isSeatLocked
        && base.userId == other.userId
        && base.userName == other.userName
        && base.userAvatar == other.userAvatar
        && base.userMicrophoneStatus == other.userMicrophoneStatus
        && base.userCameraStatus == other.userCameraStatus
        && base.userSuspendStatus == other.userSuspendStatus
        && base.x == other.x
        && base.y == other.y
        && base.width == other.width
        && base.height == other.height
        && base.zorder == other.zorder
    }
}

fileprivate var templateIdKeyTag: UInt8 = 0
extension TUISeatFullInfo {
    var templateId: UInt? {
        set {
            objc_setAssociatedObject(self, &templateIdKeyTag, newValue, .OBJC_ASSOCIATION_ASSIGN)
        }
        get {
            objc_getAssociatedObject(self, &templateIdKeyTag) as? UInt
        }
    }
}
