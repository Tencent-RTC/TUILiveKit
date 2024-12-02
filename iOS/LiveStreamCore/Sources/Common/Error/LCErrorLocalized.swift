//
//  LCErrorLocalized.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/11/29.
//

import RTCRoomEngine

protocol LCLocalizedError: Error {
    var lcDescription: String { get }
}

extension TUIError: LCLocalizedError {
    var lcDescription: String {
        switch self {
        case .success:
            return "liveCore.error.success".lsExt.localized
        case .failed:
            return "liveCore.error.failed".lsExt.localized
        case .freqLimit:
            return "liveCore.error.freqLimit".lsExt.localized
        case .repeatOperation:
            return "liveCore.error.repeat.operation".lsExt.localized
        case .sdkAppIDNotFound:
            return "liveCore.error.sdkAppId.notFound".lsExt.localized
        case .invalidParameter:
            return "liveCore.error.invalidParameter".lsExt.localized
        case .sdkNotInitialized:
            return "liveCore.error.sdkNotInitialized".lsExt.localized
        case .permissionDenied:
            return "liveCore.error.permissionDenied".lsExt.localized
        case .requirePayment:
            return "liveCore.error.requirePayment".lsExt.localized
        case .cameraStartFail:
            return "liveCore.error.cameraStartFail".lsExt.localized
        case .cameraNotAuthorized:
            return "liveCore.error.cameraNotAuthorized".lsExt.localized
        case .cameraOccupied:
            return "liveCore.error.cameraOccupied".lsExt.localized
        case .cameraDeviceEmpty:
            return "liveCore.error.cameraDeviceEmpty".lsExt.localized
        case .microphoneStartFail:
            return "liveCore.error.microphoneStartFail".lsExt.localized
        case .microphoneNotAuthorized:
            return "liveCore.error.microphoneNotAuthorized".lsExt.localized
        case .microphoneOccupied:
            return "liveCore.error.microphoneOccupied".lsExt.localized
        case .microphoneDeviceEmpty:
            return "liveCore.error.microphoneDeviceEmpty".lsExt.localized
        case .getScreenSharingTargetFailed:
            return "liveCore.error.getScreenSharingTargetFailed".lsExt.localized
        case .startScreenSharingFailed:
            return "liveCore.error.startScreenSharingFailed".lsExt.localized
        case .roomIdNotExist:
            return "liveCore.error.roomId.notExist".lsExt.localized
        case .operationInvalidBeforeEnterRoom:
            return "liveCore.error.operation.invalid.beforeEnterRoom".lsExt.localized
        case .exitNotSupportedForRoomOwner:
            return "liveCore.error.exitNotSupported.forRoomOwner".lsExt.localized
        case .operationNotSupportedInCurrentRoomType:
            return "liveCore.error.operation.notSupported.inCurrentSpeechMode".lsExt.localized
        case .roomIdInvalid:
            return "liveCore.error.roomId.invalid".lsExt.localized
        case .roomIdOccupied:
            return "liveCore.error.roomId.occupied".lsExt.localized
        case .roomNameInvalid:
            return "liveCore.error.roomName.invalid".lsExt.localized
        case .alreadyInOtherRoom:
            return "liveCore.error.already.in.OtherRoom".lsExt.localized
        case .userNotExist:
            return "liveCore.error.userNotExist".lsExt.localized
        case .userNotEntered:
            return "liveCore.error.userNotEntered".lsExt.localized
        case .userNeedOwnerPermission:
            return "liveCore.error.user.need.OwnerPermission".lsExt.localized
        case .userNeedAdminPermission:
            return "liveCore.error.user.need.AdminPermission".lsExt.localized
        case .requestNoPermission:
            return "liveCore.error.request.noPermission".lsExt.localized
        case .requestIdInvalid:
            return "liveCore.error.requestId.invalid".lsExt.localized
        case .requestIdRepeat:
            return "liveCore.error.repeat.requestId".lsExt.localized
        case .requestIdConflict:
            return "liveCore.error.conflict.requestId".lsExt.localized
        case .maxSeatCountLimit:
            return "liveCore.error.max.seat.count.limit".lsExt.localized
        case .alreadyInSeat:
            return "liveCore.error.already.in.seat".lsExt.localized
        case .seatOccupied:
            return "liveCore.error.seat.occupied".lsExt.localized
        case .seatLocked:
            return "liveCore.error.seat.locked".lsExt.localized
        case .seatIndexNotExist:
            return "liveCore.error.seat.index.not.exist".lsExt.localized
        case .userNotInSeat:
            return "liveCore.error.user.not.in.seat".lsExt.localized
        case .allSeatOccupied:
            return "liveCore.error.all.seat.occupied".lsExt.localized
        case .seatNotSupportLinkMic:
            return "liveCore.error.seat.not.support.link.mic".lsExt.localized
        case .openMicrophoneNeedSeatUnlock:
            return "liveCore.error.open.microphone.need.seat.unlock".lsExt.localized
        case .openMicrophoneNeedPermissionFromAdmin:
            return "liveCore.error.open.microphone.need.permission.from.admin".lsExt.localized
        case .openCameraNeedSeatUnlock:
            return "liveCore.error.open.camera.need.seat.unlock".lsExt.localized
        case .openCameraNeedPermissionFromAdmin:
            return "liveCore.error.open.camera.need.permission.from.admin".lsExt.localized
        case .openScreenShareNeedSeatUnlock:
            return "liveCore.error.open.screen.share.need.seat.unlock".lsExt.localized
        case .openScreenShareNeedPermissionFromAdmin:
            return "liveCore.error.open.screen.share.need.permission.from.admin".lsExt.localized
        case .sendMessageDisabledForAll:
            return "liveCore.error.send.message.disabled.for.all".lsExt.localized
        case .sendMessageDisabledForCurrent:
            return "liveCore.error.send.message.disabled.for.current".lsExt.localized
        default:
            return "liveCore.error.failed".lsExt.localized
        }
    }
}
