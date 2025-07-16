//
//  ErrorLocalized.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/11.
//

import Foundation
import RTCRoomEngine
import Combine
import ImSDK_Plus
import RTCCommon

public protocol LocalizedError: Error {
    var description: String { get }
}

public typealias InternalError = ErrorLocalized.OperateError

public class ErrorLocalized {
    public static let generalErrorCode = -1
    
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
    public struct OperateError: Error {
        let error: LocalizedError?
        let message: String
        var code: Int = 0
        public var localizedMessage: String {
            if let error = error {
                return error.description
            }
            return "\(internalLocalized("Temporarily Unclassified General Error")):\(code)"
        }
        
        public init(error: LocalizedError, message: String) {
            self.error = error
            self.message = message
        }
        
        public init(code: Int, message: String) {
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

public class UnknownError: Error {
    let rawValue: Int
    public init(rawValue: Int) {
        self.rawValue = rawValue
    }
}

extension UnknownError: LocalizedError {
    public var description: String {
        "\(internalLocalized("Temporarily Unclassified General Error")):\(rawValue)"
    }
}

public enum TIMError: Int, Error {
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
    public var description: String {
        switch self {
        case .success:
            return internalLocalized("Operation Successful")
        case .failed:
            return "\(internalLocalized("Temporarily Unclassified General Error")):\(rawValue)"
        case .invalidUserId:
            return internalLocalized("live.error.invalid.userId")
        case .ERR_SDK_COMM_API_CALL_FREQUENCY_LIMIT:
            return internalLocalized("Request Rate Limited, Please Try Again Later")
        case .ERR_SVR_GROUP_SHUTUP_DENY:
            return internalLocalized("You Have Been Muted in the Current Room")
        case .ERR_SDK_BLOCKED_BY_SENSITIVE_WORD:
            return internalLocalized("Sensitive words are detected, please modify it and try again")
        case .ERR_SDK_NET_PKG_SIZE_LIMIT:
            return internalLocalized("The content is too long, please reduce the content and try again")
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
            return internalLocalized("The network is abnormal, please try again later")
        }
    }
}

extension TUIBattleCode: LocalizedError {
    public var description: String {
        switch self {
            case .unknown:
                return internalLocalized("Temporarily Unclassified General Error")
            case .success:
                return internalLocalized("Operation Successful")
            case .battlingOtherRoom:
                return internalLocalized("The anchor is in the battle and cannot initiate the battle")
            default:
                return internalLocalized("The other error, cannot initiate the battle")
        }
    }
}

extension TUIConnectionCode: LocalizedError {
    public var description: String {
        switch self {
        case .success:
            return internalLocalized("Operation Successful")
        case .roomNotExist:
            return internalLocalized("The room you are invited to connect to does not exist")
        case .connecting:
            return internalLocalized("The room you are invited to connect to is already in the invitation list or is already connected.")
        case .connectingOtherRoom:
            return internalLocalized("The room you are invited to connect to is connected to another room.")
        case .full:
            return internalLocalized("The number of co-hosting has exceeded the maximum limit.")
        case .retry:
            return internalLocalized("Internal error, it is recommended to try again.")
        default:
            return internalLocalized("Temporarily Unclassified General Error")
        }
    }
}

public enum LiveError: Int, Error {
    case success = 0
    case freqLimit = -2
    case repeatOperation = -3
    case roomMismatch = -4
    case sdkAppIDNotFound = -1000
    case invalidParameter = -1001
    case sdkNotInitialized = -1002
    case permissionDenied = -1003
    case requirePayment = -1004
    case invalidLicense = -1005
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
    case callInProgress = -6001
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
    case giftAbilityNotEnabled = 102001
    case giftNotExist = 102002
    case giftServerPreVerificationFailed = 102004
    
    // TIMError
    case ERR_SDK_COMM_TINYID_EMPTY = 7002
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
    case ERR_SVR_GROUP_NOT_FOUND = 10010
    case ERR_INVALID_PARAMETERS = 6017

}

extension LiveError: LocalizedError {
    public var description: String {
        switch self {
        case .success:
            return internalLocalized("Operation Successful")
        case .freqLimit:
            return internalLocalized("Request Rate Limited, Please Try Again Later")
        case .repeatOperation:
            return internalLocalized("Repeat Operation")
        case .sdkAppIDNotFound:
            return internalLocalized("Not Found SDKAppID, Please Confirm Application Info in TRTC Console")
        case .invalidParameter:
            return internalLocalized("Passing illegal parameters when calling API, check if the parameters are legal")
        case .sdkNotInitialized:
            return internalLocalized("Not Logged In, Please Call Login API")
        case .permissionDenied:
            return internalLocalized("Failed to Obtain Permission, Unauthorized Audio/Video Permission, Please Check if Device Permission is Enabled")
        case .requirePayment:
            return internalLocalized("This feature requires an additional package. Please activate the corresponding package as needed in the TRTC Console")
        case .invalidLicense:
            return internalLocalized("Playback authentication failed. Please check whether the date and time settings on your phone are correct")
        case .cameraStartFail:
            return internalLocalized("System Issue, Failed to Open Camera. Check if Camera Device is Normal")
        case .cameraNotAuthorized:
            return internalLocalized("Camera has No System Authorization, Check System Authorization")
        case .cameraOccupied:
            return internalLocalized("Camera is Occupied, Check if Other Process is Using Camera")
        case .cameraDeviceEmpty:
            return internalLocalized("No Camera Device Currently, Please Insert Camera Device to Solve the Problem")
        case .microphoneStartFail:
            return internalLocalized("System Issue, Failed to Open Mic. Check if Mic Device is Normal")
        case .microphoneNotAuthorized:
            return internalLocalized("Mic has No System Authorization, Check System Authorization")
        case .microphoneOccupied:
            return internalLocalized("Mic is Occupied")
        case .microphoneDeviceEmpty:
            return internalLocalized("No Mic Device Currently")
        case .getScreenSharingTargetFailed:
            return internalLocalized("Failed to get screen sharing source (screen and window), check screen recording permissions")
        case .startScreenSharingFailed:
            return internalLocalized("Failed to Enable Screen Sharing, Check if Someone is Already Screen Sharing in the Room")
        case .operationInvalidBeforeEnterRoom:
            return internalLocalized("This Feature Can Only Be Used After Entering the Room")
        case .exitNotSupportedForRoomOwner:
            return internalLocalized("Room Owner Does Not Support Leaving the Room, Room Owner Can Only Close the Room")
        case .operationNotSupportedInCurrentRoomType:
            return internalLocalized("This Operation is Not Supported in the Current Room Type")
        case .roomIdInvalid:
            return internalLocalized("Illegal Custom Room ID, Must Be Printable ASCII Characters (0x20-0x7e), Up to 48 Bytes Long")
        case .roomNameInvalid:
            return internalLocalized("Illegal Room Name, Maximum 30 Bytes, Must Be UTF-8 Encoding if Contains Chinese Characters")
        case .alreadyInOtherRoom:
            return internalLocalized("User is Already in Another Room, Single RoomEngine Instance Only Supports User Entering One Room, To Enter Different Room, Please Leave the Room or Use New RoomEngine Instance")
        case .userNotExist:
            return internalLocalized("User is not exist")
        case .userNeedOwnerPermission:
            return internalLocalized("Room Owner Permission Required for Operation")
        case .userNeedAdminPermission:
            return internalLocalized("Room Owner or Administrator Permission Required for Operation")
        case .requestNoPermission:
            return internalLocalized("No Permission for Signaling Request, e.g. Canceling an Invite Not Initiated by Yourself")
        case .requestIdInvalid:
            return internalLocalized("Signaling Request ID is Invalid or Has Been Processed")
        case .requestIdRepeat:
            return internalLocalized("Signal request repetition")
        case .maxSeatCountLimit:
            return internalLocalized("Maximum Seat Exceeds Package Quantity Limit")
        case .seatIndexNotExist:
            return internalLocalized("Seat Serial Number Does Not Exist")
        case .openMicrophoneNeedSeatUnlock:
            return internalLocalized("Current Seat Audio is Locked")
        case .openMicrophoneNeedPermissionFromAdmin:
            return internalLocalized("Need to Apply to Room Owner or Administrator to Open Mic")
        case .openCameraNeedSeatUnlock:
            return internalLocalized("Current Seat Video is Locked, Need Room Owner to Unlock Mic Seat Before Opening Camera")
        case .openCameraNeedPermissionFromAdmin:
            return internalLocalized("Need to Apply to Room Owner or Administrator to Open Camera")
        case .openScreenShareNeedSeatUnlock:
            return internalLocalized("The current microphone position video is locked and needs to be unlocked by the room owner before screen sharing can be enabled")
        case .openScreenShareNeedPermissionFromAdmin:
            return internalLocalized("Screen sharing needs to be enabled after applying to the room owner or administrator")
        case .sendMessageDisabledForAll:
            return internalLocalized("All Members Muted in the Current Room")
        case .sendMessageDisabledForCurrent:
            return internalLocalized("You Have Been Muted in the Current Room")
        case .roomNotSupportPreloading:
            return internalLocalized("The current room does not support preloading")
        case .callInProgress:
            return internalLocalized("The device operation failed while in a call")
        case .systemInternalError:  // 100001
            return internalLocalized("Server internal error, please retry")
        case .paramIllegal:     // 100002
            return internalLocalized("The parameter is illegal. Check whether the request is correct according to the error description")
        case .roomIdOccupied:   // 100003
            return internalLocalized("The room ID already exists. Please select another room ID")
        case .roomIdNotExist:   // 100004
            return internalLocalized("The room does not exist, or it once existed but has now been dissolved")
        case .userNotEntered:   // 100005
            return internalLocalized("Not a room member")
        case .insufficientOperationPermissions: // 100006
            return internalLocalized("You do not have permission to perform this operation")
        case .noPaymentInformation: // 100007
            return internalLocalized("No payment information, you need to purchase a package in the console")
        case .roomIsFull:   // 100008
            return internalLocalized("The room is full")
        case .tagQuantityExceedsUpperLimit: // 100009
            return internalLocalized("Tag quantity Exceeds Upper limit")
        case .roomIdHasBeenUsed:    // 100010
            return internalLocalized("The room ID has been used, and the operator is the room owner, it can be used directly")
        case .roomIdHasBeenOccupiedByChat:  // 100011
            return internalLocalized("The room ID has been occupied by Chat. You can use a different room ID or dissolve the group first")
        case .creatingRoomsExceedsTheFrequencyLimit:    // 100012
            return internalLocalized("Creating rooms exceeds the frequency limit, the same room ID can only be created once within 1 second")
        case .exceedsTheUpperLimit:     // 100013
            return internalLocalized("Exceeds the upper limit, for example, the number of microphone seats, the number of PK match rooms, etc., exceeds the payment limit")
        case .invalidRoomType:  // 100015
            return internalLocalized("Invalid room type")
        case .memberHasBeenBanned:  // 100016
            return internalLocalized("This member has been banned")
        case .memberHasBeenMuted:   // 100017
            return internalLocalized("This member has been muted")
        case .requiresPassword:     // 100018
            return internalLocalized("The current room requires a password for entry")
        case .roomEntryPasswordError:   // 100019
            return internalLocalized("Room Entry Password Error")
        case .roomAdminQuantityExceedsTheUpperLimit:    // 100020
            return internalLocalized("The admin quantity exceeds the upper limit")
        case .requestIdConflict:    // 100102
            return internalLocalized("Signal request conflict")
        case .seatLocked:   // 100200
            return internalLocalized("The seat is locked. You can try another seat")
        case .seatOccupied:     // 100201
            return internalLocalized("The current seat is already occupied")
        case .alreadyOnTheSeatQueue:    // 100202
            return internalLocalized("Already on the seat queue")
        case .alreadyInSeat:    // 100203
            return internalLocalized("Already on the seat")
        case .notOnTheSeatQueue:    // 100204
            return internalLocalized("Not on the seat queue")
        case .allSeatOccupied:  // 100205
            return internalLocalized("The seats are all taken.")
        case .userNotInSeat:    // 100206
            return internalLocalized("Not on the seat")
        case .userAlreadyOnSeat:    // 100210
            return internalLocalized("The user is already on the seat")
        case .seatNotSupportLinkMic:    // 100211
            return internalLocalized("The room does not support seat ability")
        case .emptySeatList:    // 100251
            return internalLocalized("The seat list is empty")
        case .connectionNotExist:   // 100400
            return internalLocalized("The current connection does not exist or has ended")
        case .roomInConnection:     // 100401
            return internalLocalized("The room is already in connection")
        case .pendingConnectionRequest:     // 100402
            return internalLocalized("There is a pending connection request for this room")
        case .roomConnectedInOther:     // 100403
            return internalLocalized("The current room is connecting with other rooms")
        case .connectionOrBattleLimitExceeded:  // 100404
            return internalLocalized("The room number has exceeded the limit in connection or battle")
        case .creatingConnectionTooFrequent:    // 100405
            return internalLocalized("creating connections too frequent in a short time. Wait a moment and try again")
        case .battleNotExistOrEnded:    // 100411
            return internalLocalized("The battle does not exist or has ended")
        case .noRoomsInBattleIsValid:   // 100412
            return internalLocalized("None of the rooms in the battle is valid")
        case .creatingBattleTooFrequently:  // 100413
            return internalLocalized("creating battles too frequently. Wait a moment and try again")
        case .roomNotInBattle:  // 100414
            return internalLocalized("The room isn‘t in the battle")
        case .inOtherBattle:    // 100415
            return internalLocalized("The room is already in other battle")
        case .pendingBattleRequest:     // 100416
            return internalLocalized("There is a pending battle request for this room")
        case .notAllowedCancelBattleForRoomInBattle:    // 100419
            return internalLocalized("It's not allowed to cancel battle for room in battle")
        case .battleNotStart:   // 100420
            return internalLocalized("The battle has not started yet")
        case .battleHasEnded:   // 100421
            return internalLocalized("The battle session has ended")
        case .metadataKeyExceedsLimit:  // 100500
            return internalLocalized("The number of keys in the room's Metadata exceeds the limit")
        case .metadataValueSizeExceedsByteLimit:  // 100501
            return internalLocalized("The size of value in the room's Metadata exceeds the maximum byte limit")
        case .metadataTotalValueSizeExceedsByteLimit:  // 100502
            return internalLocalized("The total size of all value in the room's Metadata exceeds the maximum byte limit")
        case .metadataNoValidKey:  // 100503
            return internalLocalized("There is no valid keys when delete metadata")
        case .metadataKeySizeExceedsByteLimit:  // 100504
            return internalLocalized("The size of key in the room's Metadata exceeds the maximum byte limit")
        case .giftAbilityNotEnabled: // 102001
            return internalLocalized("Gift service is not enabled yet please check your package version")
        case .giftNotExist: // 102002
            return internalLocalized("Gift does not exist")
        case .giftServerPreVerificationFailed: // 102004
            return internalLocalized("Gift server pre-verification failed please check console configuration")
            
        // TIMError
        case .ERR_SDK_COMM_TINYID_EMPTY:
            return internalLocalized("Invalid userId")
        case .ERR_SDK_COMM_API_CALL_FREQUENCY_LIMIT:
            return internalLocalized("Request Rate Limited, Please Try Again Later")
        case .ERR_SVR_GROUP_SHUTUP_DENY:
            return internalLocalized("You Have Been Muted in the Current Room")
        case .ERR_SDK_BLOCKED_BY_SENSITIVE_WORD:
            return internalLocalized("Sensitive words are detected, please modify it and try again")
        case .ERR_SDK_NET_PKG_SIZE_LIMIT:
            return internalLocalized("The content is too long, please reduce the content and try again")
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
            return internalLocalized("The network is abnormal, please try again later")
            
        @unknown default:
            return internalLocalized("Temporarily Unclassified General Error") + ":\(self.rawValue)"
        }
    }
}
