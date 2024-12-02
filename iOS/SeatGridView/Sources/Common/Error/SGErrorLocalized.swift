//
//  ErrorLocalized.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/21.
//

import Foundation
import RTCRoomEngine

protocol LocalizedError: Error {
    var description: String { get }
}

extension TUIError: LocalizedError {
    var description: String {
        switch self {
            case .success:
                return "voiceCore.error.success".vrExt.localized
            case .failed:
                return "voiceCore.error.failed".vrExt.localized
            case .freqLimit:
                return "voiceCore.error.freqLimit".vrExt.localized
            case .repeatOperation:
                return "voiceCore.error.repeat.operation".vrExt.localized
            case .sdkAppIDNotFound:
                return "voiceCore.error.sdkAppId.notFound".vrExt.localized
            case .invalidParameter:
                return "voiceCore.error.invalidParameter".vrExt.localized
            case .sdkNotInitialized:
                return "voiceCore.error.sdkNotInitialized".vrExt.localized
            case .permissionDenied:
                return "voiceCore.error.permissionDenied".vrExt.localized
            case .requirePayment:
                return "voiceCore.error.requirePayment".vrExt.localized
            case .cameraStartFail:
                return "voiceCore.error.cameraStartFail".vrExt.localized
            case .cameraNotAuthorized:
                return "voiceCore.error.cameraNotAuthorized".vrExt.localized
            case .cameraOccupied:
                return "voiceCore.error.cameraOccupied".vrExt.localized
            case .cameraDeviceEmpty:
                return "voiceCore.error.cameraDeviceEmpty".vrExt.localized
            case .microphoneStartFail:
                return "voiceCore.error.microphoneStartFail".vrExt.localized
            case .microphoneNotAuthorized:
                return "voiceCore.error.microphoneNotAuthorized".vrExt.localized
            case .microphoneOccupied:
                return "voiceCore.error.microphoneOccupied".vrExt.localized
            case .microphoneDeviceEmpty:
                return "voiceCore.error.microphoneDeviceEmpty".vrExt.localized
            case .getScreenSharingTargetFailed:
                return "voiceCore.error.getScreenSharingTargetFailed".vrExt.localized
            case .startScreenSharingFailed:
                return "voiceCore.error.startScreenSharingFailed".vrExt.localized
            case .roomIdNotExist:
                return "voiceCore.error.roomId.notExist".vrExt.localized
            case .operationInvalidBeforeEnterRoom:
                return "voiceCore.error.operation.invalid.beforeEnterRoom".vrExt.localized
            case .exitNotSupportedForRoomOwner:
                return "voiceCore.error.exitNotSupported.forRoomOwner".vrExt.localized
            case .operationNotSupportedInCurrentRoomType:
                return "voiceCore.error.operation.notSupported.inCurrentSpeechMode".vrExt.localized
            case .roomIdInvalid:
                return "voiceCore.error.roomId.invalid".vrExt.localized
            case .roomIdOccupied:
                return "voiceCore.error.roomId.occupied".vrExt.localized
            case .roomNameInvalid:
                return "voiceCore.error.roomName.invalid".vrExt.localized
            case .alreadyInOtherRoom:
                return "voiceCore.error.already.in.OtherRoom".vrExt.localized
            case .userNotExist:
                return "voiceCore.error.userNotExist".vrExt.localized
            case .userNotEntered:
                return "voiceCore.error.userNotEntered".vrExt.localized
            case .userNeedOwnerPermission:
                return "voiceCore.error.user.need.OwnerPermission".vrExt.localized
            case .userNeedAdminPermission:
                return "voiceCore.error.user.need.AdminPermission".vrExt.localized
            case .requestNoPermission:
                return "voiceCore.error.request.noPermission".vrExt.localized
            case .requestIdInvalid:
                return "voiceCore.error.requestId.invalid".vrExt.localized
            case .requestIdRepeat:
                return "voiceCore.error.repeat.requestId".vrExt.localized
            case .requestIdConflict:
                return "voiceCore.error.conflict.requestId".vrExt.localized
            case .maxSeatCountLimit:
                return "voiceCore.error.max.seat.count.limit".vrExt.localized
            case .alreadyInSeat:
                return "voiceCore.error.already.in.seat".vrExt.localized
            case .seatOccupied:
                return "voiceCore.error.seat.occupied".vrExt.localized
            case .seatLocked:
                return "voiceCore.error.seat.locked".vrExt.localized
            case .seatIndexNotExist:
                return "voiceCore.error.seat.index.not.exist".vrExt.localized
            case .userNotInSeat:
                return "voiceCore.error.user.not.in.seat".vrExt.localized
            case .allSeatOccupied:
                return "voiceCore.error.all.seat.occupied".vrExt.localized
            case .seatNotSupportLinkMic:
                return "voiceCore.error.seat.not.support.link.mic".vrExt.localized
            case .openMicrophoneNeedSeatUnlock:
                return "voiceCore.error.open.microphone.need.seat.unlock".vrExt.localized
            case .openMicrophoneNeedPermissionFromAdmin:
                return "voiceCore.error.open.microphone.need.permission.from.admin".vrExt.localized
            case .openCameraNeedSeatUnlock:
                return "voiceCore.error.open.camera.need.seat.unlock".vrExt.localized
            case .openCameraNeedPermissionFromAdmin:
                return "voiceCore.error.open.camera.need.permission.from.admin".vrExt.localized
            case .openScreenShareNeedSeatUnlock:
                return "voiceCore.error.open.screen.share.need.seat.unlock".vrExt.localized
            case .openScreenShareNeedPermissionFromAdmin:
                return "voiceCore.error.open.screen.share.need.permission.from.admin".vrExt.localized
            case .sendMessageDisabledForAll:
                return "voiceCore.error.send.message.disabled.for.all".vrExt.localized
            case .sendMessageDisabledForCurrent:
                return "voiceCore.error.send.message.disabled.for.current".vrExt.localized
            default:
                return "voiceCore.error.failed".vrExt.localized
        }
    }
}

