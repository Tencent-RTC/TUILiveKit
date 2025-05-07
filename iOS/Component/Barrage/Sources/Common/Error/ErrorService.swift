//
//  VRErrorService.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation
import RTCRoomEngine
import Combine
import ImSDK_Plus
import RTCCommon

protocol LocalizedError: Error {
    var description: String { get }
}

typealias InternalError = ErrorService.OperateError

class ErrorService {
    static let generalErrorCode = -1
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    /// OperateError
    /// - property:
    ///     - error: TUIError, RoomEngine throw.
    ///     - message: RoomEngine throw
    ///     - localizedMessage: covert to localized string.
    ///     - actions: if you want to dispatch actions after receive error, append action to this property.
    ///     store will filter error action and dispath others.
    struct OperateError: Error {
        let error: LocalizedError?
        let message: String
        var code: Int = 0
        var localizedMessage: String {
            if let error = error {
                return error.description
            }
            return "code: \(code), message: \(message)"
        }
        
        init(error: LocalizedError, message: String) {
            self.error = error
            self.message = message
        }
        
        init(code: Int, message: String) {
            if let err = LiveError(rawValue: code) {
                self.error = err
            } else {
                self.error = nil
                self.code = code
            }
            self.message = message
        }
    }
}

class UnknownError: Error {
    let rawValue: Int
    init(rawValue: Int) {
        self.rawValue = rawValue
    }
}

extension UnknownError: LocalizedError {
    var description: String {
        "\(String.localized("Temporarily Unclassified General Error")):\(rawValue)"
    }
}

enum TIMError: Int, Error {
    case success = 0
    case failed = -1
    case invalidUserId = 7_002
    case ERR_SDK_COMM_API_CALL_FREQUENCY_LIMIT = 7008
    case ERR_SVR_GROUP_SHUTUP_DENY = 10017
    case ERR_SDK_BLOCKED_BY_SENSITIVE_WORD = 7015
    case ERR_SDK_NET_PKG_SIZE_LIMIT = 9522
    case ERR_SDK_NET_DISCONNECT = 9508
    case ERR_SDK_NET_WAIT_ACK_TIMEOUT = 9520
    case ERR_SDK_NET_ALLREADY_CONN = 9509
    case ERR_SDK_NET_CONN_TIMEOUT = 9510
    case ERR_SDK_NET_CONN_REFUSE = 9511
    case ERR_SDK_NET_NET_UNREACH = 9512
    case ERR_SDK_NET_WAIT_INQUEUE_TIMEOUT = 9518
    case ERR_SDK_NET_WAIT_SEND_TIMEOUT = 9519
    case ERR_SDK_NET_WAIT_SEND_REMAINING_TIMEOUT = 9521
    case ERR_SDK_NET_WAIT_SEND_TIMEOUT_NO_NETWORK = 9523
    case ERR_SDK_NET_WAIT_ACK_TIMEOUT_NO_NETWORK = 9524
    case ERR_SDK_NET_SEND_REMAINING_TIMEOUT_NO_NETWORK = 9525
}

extension TIMError: LocalizedError {
    var description: String {
        switch self {
        case .success:
            return .localized("Operation Successful")
        case .failed:
            return "\(String.localized("Temporarily Unclassified General Error")):\(rawValue)"
        case .invalidUserId:
            return .localized("live.error.invalid.userId")
        case .ERR_SDK_COMM_API_CALL_FREQUENCY_LIMIT:
            return .localized("Request Rate Limited, Please Try Again Later")
        case .ERR_SVR_GROUP_SHUTUP_DENY:
            return .localized("You Have Been Muted in the Current Room")
        case .ERR_SDK_BLOCKED_BY_SENSITIVE_WORD:
            return .localized("Sensitive words are detected, please modify it and try again")
        case .ERR_SDK_NET_PKG_SIZE_LIMIT:
            return .localized("The content is too long, please reduce the content and try again")
        case .ERR_SDK_NET_DISCONNECT,
                .ERR_SDK_NET_WAIT_ACK_TIMEOUT,
                .ERR_SDK_NET_ALLREADY_CONN,
                .ERR_SDK_NET_CONN_TIMEOUT,
                .ERR_SDK_NET_CONN_REFUSE,
                .ERR_SDK_NET_NET_UNREACH,
                .ERR_SDK_NET_WAIT_INQUEUE_TIMEOUT,
                .ERR_SDK_NET_WAIT_SEND_TIMEOUT,
                .ERR_SDK_NET_WAIT_SEND_REMAINING_TIMEOUT,
                .ERR_SDK_NET_WAIT_SEND_TIMEOUT_NO_NETWORK,
                .ERR_SDK_NET_WAIT_ACK_TIMEOUT_NO_NETWORK,
                .ERR_SDK_NET_SEND_REMAINING_TIMEOUT_NO_NETWORK:
            return .localized("The network is abnormal, please try again later")
        }
    }
}

extension TUIBattleCode: LocalizedError {
    var description: String {
        switch self {
            case .unknown:
                return .localized("Temporarily Unclassified General Error")
            case .success:
                return .localized("Operation Successful")
            case .battlingOtherRoom:
                return .localized("The anchor is in the battle and cannot initiate the battle")
            default:
                return .localized("The other error, cannot initiate the battle")
        }
    }
}

extension TUIConnectionCode: LocalizedError {
    var description: String {
        switch self {
        case .success:
            return .localized("Operation Successful")
        case .roomNotExist:
            return .localized("The room you are invited to connect to does not exist")
        case .connecting:
            return .localized("The room you are invited to connect to is already in the invitation list or is already connected.")
        case .connectingOtherRoom:
            return .localized("The room you are invited to connect to is connected to another room.")
        case .full:
            return .localized("The number of co-hosting has exceeded the maximum limit.")
        case .retry:
            return .localized("Internal error, it is recommended to try again.")
        default:
            return .localized("Temporarily Unclassified General Error")
        }
    }
}

enum LiveError: Int, Error {
    case success = 0
    case freqLimit = -2
    case repeatOperation = -3
    case roomMismatch = -4
    case sdkAppIDNotFound = -1000
    case invalidParameter = -1001
    case sdkNotInitialized = -1002
    case permissionDenied = -1003
    case requirePayment = -1004
    case cameraStartFail = -1100
    case cameraNotAuthorized = -1101
    case cameraOccupied = -1102
    case cameraDeviceEmpty = -1103
    case microphoneStartFail = -1104
    case microphoneNotAuthorized = -1105
    case microphoneOccupied = -1106
    case microphoneDeviceEmpty = -1107
    case getScreenSharingTargetFailed = -1108
    case startScreenSharingFailed = -1109
    case operationInvalidBeforeEnterRoom = -2101
    case exitNotSupportedForRoomOwner = -2102
    case operationNotSupportedInCurrentRoomType = -2103
    case roomIdInvalid = -2105
    case roomNameInvalid = -2107
    case alreadyInOtherRoom = -2108
    case userNotExist = -2200
    case userNeedOwnerPermission = -2300
    case userNeedAdminPermission = -2301
    case requestNoPermission = -2310
    case requestIdInvalid = -2311
    case requestIdRepeat = -2312
    case maxSeatCountLimit = -2340
    case seatIndexNotExist = -2344
    case openMicrophoneNeedSeatUnlock = -2360
    case openMicrophoneNeedPermissionFromAdmin = -2361
    case openCameraNeedSeatUnlock = -2370
    case openCameraNeedPermissionFromAdmin = -2371
    case openScreenShareNeedSeatUnlock = -2372
    case openScreenShareNeedPermissionFromAdmin = -2373
    case sendMessageDisabledForAll = -2380
    case sendMessageDisabledForCurrent = -2381
    case roomNotSupportPreloading = -4001
    case invalidUserId = 7_002
    case hasBeenMuted = 10017
    case systemInternalError = 100001
    case paramIllegal = 100002
    case roomIdOccupied = 100003
    case roomIdNotExist = 100004
    case userNotEntered = 100005
    case insufficientOperationPermissions = 100006
    case noPaymentInformation = 100007
    case roomIsFull = 100008
    case tagQuantityExceedsUpperLimit = 100009
    case roomIdHasBeenUsed = 100010
    case roomIdHasBeenOccupiedByChat = 100011
    case creatingRoomsExceedsTheFrequencyLimit = 100012
    case exceedsTheUpperLimit = 100013
    case invalidRoomType = 100015
    case memberHasBeenBanned = 100016
    case memberHasBeenMuted = 100017
    case requiresPassword = 100018
    case roomEntryPasswordError = 100019
    case roomAdminQuantityExceedsTheUpperLimit = 100020
    case requestIdConflict = 100102
    case seatLocked = 100200
    case seatOccupied = 100201
    case alreadyOnTheSeatQueue = 100202
    case alreadyInSeat = 100203
    case notOnTheSeatQueue = 100204
    case allSeatOccupied = 100205
    case userNotInSeat = 100206
    case userAlreadyOnSeat = 100210
    case seatNotSupportLinkMic = 100211
    case emptySeatList = 100251
    case connectionNotExist = 100400
    case roomInConnection = 100401
    case pendingConnectionRequest = 100402
    case roomConnectedInOther = 100403
    case connectionOrBattleLimitExceeded = 100404
    case creatingConnectionTooFrequent = 100405
    case battleNotExistOrEnded = 100411
    case noRoomsInBattleIsValid = 100412
    case creatingBattleTooFrequently = 100413
    case roomNotInBattle = 100414
    case inOtherBattle = 100415
    case pendingBattleRequest = 100416
    case notAllowedCancelBattleForRoomInBattle = 100419
    case battleNotStart = 100420
    case battleHasEnded = 100421
    case metadataKeyExceedsLimit = 100500
    case metadataValueSizeExceedsByteLimit = 100501
    case metadataTotalValueSizeExceedsByteLimit = 100502
    case metadataNoValidKey = 100503
    case metadataKeySizeExceedsByteLimit = 100504
}

extension LiveError: LocalizedError {
    var description: String {
        switch self {
        case .success:
            return .localized("Operation Successful")
        case .freqLimit:
            return .localized("Request Rate Limited, Please Try Again Later")
        case .repeatOperation:
            return .localized("Repeat Operation")
        case .sdkAppIDNotFound:
            return .localized("Not Found SDKAppID, Please Confirm Application Info in TRTC Console")
        case .invalidParameter:
            return .localized("Passing illegal parameters when calling API, check if the parameters are legal")
        case .sdkNotInitialized:
            return .localized("Not Logged In, Please Call Login API")
        case .permissionDenied:
            return .localized("Failed to Obtain Permission, Unauthorized Audio/Video Permission, Please Check if Device Permission is Enabled")
        case .requirePayment:
            return .localized("This feature requires an additional package. Please activate the corresponding package as needed in the TRTC Console")
        case .cameraStartFail:
            return .localized("System Issue, Failed to Open Camera. Check if Camera Device is Normal")
        case .cameraNotAuthorized:
            return .localized("Camera has No System Authorization, Check System Authorization")
        case .cameraOccupied:
            return .localized("Camera is Occupied, Check if Other Process is Using Camera")
        case .cameraDeviceEmpty:
            return .localized("No Camera Device Currently, Please Insert Camera Device to Solve the Problem")
        case .microphoneStartFail:
            return .localized("System Issue, Failed to Open Mic. Check if Mic Device is Normal")
        case .microphoneNotAuthorized:
            return .localized("Mic has No System Authorization, Check System Authorization")
        case .microphoneOccupied:
            return .localized("Mic is Occupied")
        case .microphoneDeviceEmpty:
            return .localized("No Mic Device Currently")
        case .getScreenSharingTargetFailed:
            return .localized("Failed to get screen sharing source (screen and window), check screen recording permissions")
        case .startScreenSharingFailed:
            return .localized("Failed to Enable Screen Sharing, Check if Someone is Already Screen Sharing in the Room")
        case .operationInvalidBeforeEnterRoom:
            return .localized("This Feature Can Only Be Used After Entering the Room")
        case .exitNotSupportedForRoomOwner:
            return .localized("Room Owner Does Not Support Leaving the Room, Room Owner Can Only Close the Room")
        case .operationNotSupportedInCurrentRoomType:
            return .localized("This Operation is Not Supported in the Current Room Type")
        case .roomIdInvalid:
            return .localized("Illegal Custom Room ID, Must Be Printable ASCII Characters (0x20-0x7e), Up to 48 Bytes Long")
        case .roomNameInvalid:
            return .localized("Illegal Room Name, Maximum 30 Bytes, Must Be UTF-8 Encoding if Contains Chinese Characters")
        case .alreadyInOtherRoom:
            return .localized("User is Already in Another Room, Single RoomEngine Instance Only Supports User Entering One Room, To Enter Different Room, Please Leave the Room or Use New RoomEngine Instance")
        case .userNotExist:
            return .localized("User is not exist")
        case .userNeedOwnerPermission:
            return .localized("Room Owner Permission Required for Operation")
        case .userNeedAdminPermission:
            return .localized("Room Owner or Administrator Permission Required for Operation")
        case .requestNoPermission:
            return .localized("No Permission for Signaling Request, e.g. Canceling an Invite Not Initiated by Yourself")
        case .requestIdInvalid:
            return .localized("Signaling Request ID is Invalid or Has Been Processed")
        case .requestIdRepeat:
            return .localized("Signal request repetition")
        case .maxSeatCountLimit:
            return .localized("Maximum Seat Exceeds Package Quantity Limit")
        case .seatIndexNotExist:
            return .localized("Seat Serial Number Does Not Exist")
        case .openMicrophoneNeedSeatUnlock:
            return .localized("Current Seat Audio is Locked")
        case .openMicrophoneNeedPermissionFromAdmin:
            return .localized("Need to Apply to Room Owner or Administrator to Open Mic")
        case .openCameraNeedSeatUnlock:
            return .localized("Current Seat Video is Locked, Need Room Owner to Unlock Mic Seat Before Opening Camera")
        case .openCameraNeedPermissionFromAdmin:
            return .localized("Need to Apply to Room Owner or Administrator to Open Camera")
        case .openScreenShareNeedSeatUnlock:
            return .localized("The current microphone position video is locked and needs to be unlocked by the room owner before screen sharing can be enabled")
        case .openScreenShareNeedPermissionFromAdmin:
            return .localized("Screen sharing needs to be enabled after applying to the room owner or administrator")
        case .sendMessageDisabledForAll:
            return .localized("All Members Muted in the Current Room")
        case .sendMessageDisabledForCurrent:
            return .localized("You Have Been Muted in the Current Room")
        case .roomNotSupportPreloading:
            return .localized("The current room does not support preloading")
        case .invalidUserId:
            return .localized("Invalid userId")
        case .hasBeenMuted:   // 100017
            return .localized("You Have Been Muted in the Current Room")
        case .systemInternalError:  // 100001
            return .localized("Server internal error, please retry")
        case .paramIllegal:     // 100002
            return .localized("The parameter is illegal. Check whether the request is correct according to the error description")
        case .roomIdOccupied:   // 100003
            return .localized("The room ID already exists. Please select another room ID")
        case .roomIdNotExist:   // 100004
            return .localized("The room does not exist, or it once existed but has now been dissolved")
        case .userNotEntered:   // 100005
            return .localized("Not a room member")
        case .insufficientOperationPermissions: // 100006
            return .localized("You do not have permission to perform this operation")
        case .noPaymentInformation: // 100007
            return .localized("No payment information, you need to purchase a package in the console")
        case .roomIsFull:   // 100008
            return .localized("The room is full")
        case .tagQuantityExceedsUpperLimit: // 100009
            return .localized("Tag quantity Exceeds Upper limit")
        case .roomIdHasBeenUsed:    // 100010
            return .localized("The room ID has been used, and the operator is the room owner, it can be used directly")
        case .roomIdHasBeenOccupiedByChat:  // 100011
            return .localized("The room ID has been occupied by Chat. You can use a different room ID or dissolve the group first")
        case .creatingRoomsExceedsTheFrequencyLimit:    // 100012
            return .localized("Creating rooms exceeds the frequency limit, the same room ID can only be created once within 1 second")
        case .exceedsTheUpperLimit:     // 100013
            return .localized("Exceeds the upper limit, for example, the number of microphone seats, the number of PK match rooms, etc., exceeds the payment limit")
        case .invalidRoomType:  // 100015
            return .localized("Invalid room type")
        case .memberHasBeenBanned:  // 100016
            return .localized("This member has been banned")
        case .memberHasBeenMuted:   // 100017
            return .localized("This member has been muted")
        case .requiresPassword:     // 100018
            return .localized("The current room requires a password for entry")
        case .roomEntryPasswordError:   // 100019
            return .localized("Room Entry Password Error")
        case .roomAdminQuantityExceedsTheUpperLimit:    // 100020
            return .localized("The admin quantity exceeds the upper limit")
        case .requestIdConflict:    // 100102
            return .localized("Signal request conflict")
        case .seatLocked:   // 100200
            return .localized("The seat is locked. You can try another seat")
        case .seatOccupied:     // 100201
            return .localized("The current seat is already occupied")
        case .alreadyOnTheSeatQueue:    // 100202
            return .localized("Already on the seat queue")
        case .alreadyInSeat:    // 100203
            return .localized("Already on the seat")
        case .notOnTheSeatQueue:    // 100204
            return .localized("Not on the seat queue")
        case .allSeatOccupied:  // 100205
            return .localized("The seats are all taken.")
        case .userNotInSeat:    // 100206
            return .localized("Not on the seat")
        case .userAlreadyOnSeat:    // 100210
            return .localized("The user is already on the seat")
        case .seatNotSupportLinkMic:    // 100211
            return .localized("The room does not support seat ability")
        case .emptySeatList:    // 100251
            return .localized("The seat list is empty")
        case .connectionNotExist:   // 100400
            return .localized("The current connection does not exist or has ended")
        case .roomInConnection:     // 100401
            return .localized("The room is already in connection")
        case .pendingConnectionRequest:     // 100402
            return .localized("There is a pending connection request for this room")
        case .roomConnectedInOther:     // 100403
            return .localized("The current room is connecting with other rooms")
        case .connectionOrBattleLimitExceeded:  // 100404
            return .localized("The room number has exceeded the limit in connection or battle")
        case .creatingConnectionTooFrequent:    // 100405
            return .localized("creating connections too frequent in a short time. Wait a moment and try again")
        case .battleNotExistOrEnded:    // 100411
            return .localized("The battle does not exist or has ended")
        case .noRoomsInBattleIsValid:   // 100412
            return .localized("None of the rooms in the battle is valid")
        case .creatingBattleTooFrequently:  // 100413
            return .localized("creating battles too frequently. Wait a moment and try again")
        case .roomNotInBattle:  // 100414
            return .localized("The room isn‘t in the battle")
        case .inOtherBattle:    // 100415
            return .localized("The room is already in other battle")
        case .pendingBattleRequest:     // 100416
            return .localized("There is a pending battle request for this room")
        case .notAllowedCancelBattleForRoomInBattle:    // 100419
            return .localized("It's not allowed to cancel battle for room in battle")
        case .battleNotStart:   // 100420
            return .localized("The battle has not started yet")
        case .battleHasEnded:   // 100421
            return .localized("The battle session has ended")
        case .metadataKeyExceedsLimit:  // 100500
            return .localized("The number of keys in the room's Metadata exceeds the limit")
        case .metadataValueSizeExceedsByteLimit:  // 100501
            return .localized("The size of value in the room's Metadata exceeds the maximum byte limit")
        case .metadataTotalValueSizeExceedsByteLimit:  // 100502
            return .localized("The total size of all value in the room's Metadata exceeds the maximum byte limit")
        case .metadataNoValidKey:  // 100503
            return .localized("There is no valid keys when delete metadata")
        case .metadataKeySizeExceedsByteLimit:  // 100504
            return .localized("The size of key in the room's Metadata exceeds the maximum byte limit")
        @unknown default:
            return .localized("Temporarily Unclassified General Error") + ", code: \(self.rawValue)"
        }
    }
}
