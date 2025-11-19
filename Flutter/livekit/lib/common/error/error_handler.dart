import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../index.dart';

class ErrorHandler {
  static const Set<LiveError?> interceptToastOnlyPrintLog = {
    LiveError.freqLimit,
    LiveError.repeatOperation,
    LiveError.seatNotSupportLinkMic,
  };

  static void onError(int code, String? message) {
    const success = 0;
    if (code == success) return;
    String? localizedMessage = convertToErrorMessage(code, message);
    if (!interceptToastOnlyPrintLog.contains(LiveError.fromInt(code)) && localizedMessage != null) {
      makeToast(msg: localizedMessage);
    }
  }

  static void handleMessage(String message) {
    makeToast(msg: message);
  }

  static String? convertToErrorMessage(int code, String? message) {
    LiveKitLogger.info('ErrorHandler :[error: $code, message: $message]');
    final liveError = LiveError.fromInt(code);
    if (liveError != null) {
      return liveError.description;
    }
    final imError = TIMError.fromInt(code);
    if (imError != null) {
      return imError.description;
    }
    return "code: $code, message: $message";
  }

  static String? convertToConnectionErrorMessage(TUIConnectionCode connectionCode) {
    LiveKitLogger.info('ErrorHandler :[error: $connectionCode]');
    return connectionCode.description;
  }

  static String? convertToBattleErrorMessage(TUIBattleCode battleCode) {
    LiveKitLogger.info('ErrorHandler :[error: $battleCode]');
    return battleCode.description;
  }
}

enum LiveError {
  success(0),
  freqLimit(-2),
  repeatOperation(-3),
  roomMismatch(-4),
  sdkAppIDNotFound(-1000),
  invalidParameter(-1001),
  sdkNotInitialized(-1002),
  permissionDenied(-1003),
  requirePayment(-1004),
  cameraStartFail(-1100),
  cameraNotAuthorized(-1101),
  cameraOccupied(-1102),
  cameraDeviceEmpty(-1103),
  microphoneStartFail(-1104),
  microphoneNotAuthorized(-1105),
  microphoneOccupied(-1106),
  microphoneDeviceEmpty(-1107),
  getScreenSharingTargetFailed(-1108),
  startScreenSharingFailed(-1109),
  operationInvalidBeforeEnterRoom(-2101),
  exitNotSupportedForRoomOwner(-2102),
  operationNotSupportedInCurrentRoomType(-2103),
  roomIdInvalid(-2105),
  roomNameInvalid(-2107),
  alreadyInOtherRoom(-2108),
  userNotExist(-2200),
  userNeedOwnerPermission(-2300),
  userNeedAdminPermission(-2301),
  requestNoPermission(-2310),
  requestIdInvalid(-2311),
  requestIdRepeat(-2312),
  maxSeatCountLimit(-2340),
  seatIndexNotExist(-2344),
  openMicrophoneNeedSeatUnlock(-2360),
  openMicrophoneNeedPermissionFromAdmin(-2361),
  openCameraNeedSeatUnlock(-2370),
  openCameraNeedPermissionFromAdmin(-2371),
  openScreenShareNeedSeatUnlock(-2372),
  openScreenShareNeedPermissionFromAdmin(-2373),
  sendMessageDisabledForAll(-2380),
  sendMessageDisabledForCurrent(-2381),
  roomNotSupportPreloading(-4001),
  invalidUserId(7002),
  hasBeenMuted(10017),
  systemInternalError(100001),
  paramIllegal(100002),
  roomIdOccupied(100003),
  roomIdNotExist(100004),
  userNotEntered(100005),
  insufficientOperationPermissions(100006),
  noPaymentInformation(100007),
  roomIsFull(100008),
  tagQuantityExceedsUpperLimit(100009),
  roomIdHasBeenUsed(100010),
  roomIdHasBeenOccupiedByChat(100011),
  creatingRoomsExceedsTheFrequencyLimit(100012),
  exceedsTheUpperLimit(100013),
  invalidRoomType(100015),
  memberHasBeenBanned(100016),
  memberHasBeenMuted(100017),
  requiresPassword(100018),
  roomEntryPasswordError(100019),
  roomAdminQuantityExceedsTheUpperLimit(100020),
  requestIdConflict(100102),
  seatLocked(100200),
  seatOccupied(100201),
  alreadyOnTheSeatQueue(100202),
  alreadyInSeat(100203),
  notOnTheSeatQueue(100204),
  allSeatOccupied(100205),
  userNotInSeat(100206),
  userAlreadyOnSeat(100210),
  seatNotSupportLinkMic(100211),
  emptySeatList(100251),
  connectionNotExist(100400),
  roomInConnection(100401),
  pendingConnectionRequest(100402),
  roomConnectedInOther(100403),
  connectionOrBattleLimitExceeded(100404),
  creatingConnectionTooFrequent(100405),
  battleNotExistOrEnded(100411),
  noRoomsInBattleIsValid(100412),
  creatingBattleTooFrequently(100413),
  roomNotInBattle(100414),
  inOtherBattle(100415),
  pendingBattleRequest(100416),
  notAllowedCancelBattleForRoomInBattle(100419),
  battleNotStart(100420),
  battleHasEnded(100421),
  metadataKeyExceedsLimit(100500),
  metadataValueSizeExceedsByteLimit(100501),
  metadataTotalValueSizeExceedsByteLimit(100502),
  metadataNoValidKey(100503),
  metadataKeySizeExceedsByteLimit(100504);

  final int code;

  const LiveError(this.code);

  static LiveError? fromInt(int code) {
    for (var enumValue in LiveError.values) {
      if (enumValue.code == code) return enumValue;
    }
    return null;
  }
}

extension TUIBattleCodeWithLocalization on TUIBattleCode {
  String? get description {
    switch (this) {
      case TUIBattleCode.unknown:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_failed;
      case TUIBattleCode.success:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_success;
      case TUIBattleCode.battlingOtherRoom:
        return LiveKitLocalizations.of(Global.appContext())?.livestreamcore_battle_error_conflict;
      default:
        return LiveKitLocalizations.of(Global.appContext())?.livestreamcore_battle_error_other;
    }
  }
}

extension TUIConnectionCodeWithLocalization on TUIConnectionCode {
  String? get description {
    switch (this) {
      case TUIConnectionCode.success:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_success;
      case TUIConnectionCode.roomNotExists:
        return LiveKitLocalizations.of(Global.appContext())?.live_error_connection_notexit;
      case TUIConnectionCode.connecting:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_connection_connecting;
      case TUIConnectionCode.connectingOtherRoom:
        return LiveKitLocalizations.of(Global.appContext())?.common_connect_conflict;
      case TUIConnectionCode.connectionFull:
        return LiveKitLocalizations.of(Global.appContext())?.common_connection_room_full;
      case TUIConnectionCode.retry:
        return LiveKitLocalizations.of(Global.appContext())?.live_error_connection_retry;
      default:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_failed;
    }
  }
}

extension LiveErrorWithLocalization on LiveError {
  String? get description {
    switch (this) {
      case LiveError.success:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_success;
      case LiveError.freqLimit:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_freq_limit;
      case LiveError.repeatOperation:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_repeat_operation;
      case LiveError.sdkAppIDNotFound:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_sdk_app_id_not_found;
      case LiveError.invalidParameter:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_invalid_parameter;
      case LiveError.sdkNotInitialized:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_sdk_not_initialized;
      case LiveError.permissionDenied:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_permission_denied;
      case LiveError.requirePayment:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_require_payment;
      case LiveError.cameraStartFail:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_camera_start_fail;
      case LiveError.cameraNotAuthorized:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_camera_not_authorized;
      case LiveError.cameraOccupied:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_camera_occupied;
      case LiveError.cameraDeviceEmpty:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_camera_device_empty;
      case LiveError.microphoneStartFail:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_microphone_start_fail;
      case LiveError.microphoneNotAuthorized:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_microphone_not_authorized;
      case LiveError.microphoneOccupied:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_microphone_occupied;
      case LiveError.microphoneDeviceEmpty:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_microphone_device_empty;
      case LiveError.getScreenSharingTargetFailed:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_get_screen_sharing_target_failed;
      case LiveError.startScreenSharingFailed:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_start_screen_sharing_failed;
      case LiveError.operationInvalidBeforeEnterRoom:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_operation_invalid_before_enter_room;
      case LiveError.exitNotSupportedForRoomOwner:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_exit_not_supported_for_room_owner;
      case LiveError.operationNotSupportedInCurrentRoomType:
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_client_error_operation_not_supported_in_current_room_type;
      case LiveError.roomIdInvalid:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_room_id_invalid;
      case LiveError.roomNameInvalid:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_room_name_invalid;
      case LiveError.alreadyInOtherRoom:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_already_in_other_room;
      case LiveError.userNotExist:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_user_not_exist;
      case LiveError.userNeedOwnerPermission:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_user_need_owner_permission;
      case LiveError.userNeedAdminPermission:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_user_need_admin_permission;
      case LiveError.requestNoPermission:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_request_no_permission;
      case LiveError.requestIdInvalid:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_request_id_invalid;
      case LiveError.requestIdRepeat:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_request_id_repeat;
      case LiveError.maxSeatCountLimit:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_max_seat_count_limit;
      case LiveError.seatIndexNotExist:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_seat_index_not_exist;
      case LiveError.openMicrophoneNeedSeatUnlock:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_open_microphone_need_seat_unlock;
      case LiveError.openMicrophoneNeedPermissionFromAdmin:
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_client_error_open_microphone_need_permission_from_admin;
      case LiveError.openCameraNeedSeatUnlock:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_open_camera_need_seat_unlock;
      case LiveError.openCameraNeedPermissionFromAdmin:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_open_camera_need_permission_from_admin;
      case LiveError.openScreenShareNeedSeatUnlock:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_open_screen_share_need_seat_unlock;
      case LiveError.openScreenShareNeedPermissionFromAdmin:
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_client_error_open_screen_share_need_permission_from_admin;
      case LiveError.sendMessageDisabledForAll:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_send_message_disabled_for_all;
      case LiveError.sendMessageDisabledForCurrent:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_send_message_disabled_for_current;
      case LiveError.roomNotSupportPreloading:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_room_not_support_preloading;
      case LiveError.invalidUserId:
        return LiveKitLocalizations.of(Global.appContext())?.live_invalid_userId;
      case LiveError.hasBeenMuted: // 100017
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_send_message_disabled_for_current;
      case LiveError.systemInternalError: // 100001
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_room_not_support_preloading;
      case LiveError.paramIllegal: // 100002
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_param_illegal;
      case LiveError.roomIdOccupied: // 100003
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_room_id_exists;
      case LiveError.roomIdNotExist: // 100004
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_room_does_not_exist;
      case LiveError.userNotEntered: // 100005
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_not_a_room_member;
      case LiveError.insufficientOperationPermissions: // 100006
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_insufficient_operation_permissions;
      case LiveError.noPaymentInformation: // 100007
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_no_payment_information;
      case LiveError.roomIsFull: // 100008
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_room_is_full;
      case LiveError.tagQuantityExceedsUpperLimit: // 100009
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_tag_quantity_exceeds_upper_limit;
      case LiveError.roomIdHasBeenUsed: // 100010
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_room_id_has_been_used;
      case LiveError.roomIdHasBeenOccupiedByChat: // 100011
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_room_id_has_been_occupied_by_chat;
      case LiveError.creatingRoomsExceedsTheFrequencyLimit: // 100012
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_server_error_creating_rooms_exceeds_the_frequency_limit;
      case LiveError.exceedsTheUpperLimit: // 100013
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_exceeds_the_upper_limit;
      case LiveError.invalidRoomType: // 100015
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_invalid_room_type;
      case LiveError.memberHasBeenBanned: // 100016
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_this_member_has_been_banned;
      case LiveError.memberHasBeenMuted: // 100017
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_this_member_has_been_muted;
      case LiveError.requiresPassword: // 100018
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_requires_password;
      case LiveError.roomEntryPasswordError: // 100019
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_room_entry_password_error;
      case LiveError.roomAdminQuantityExceedsTheUpperLimit: // 100020
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_server_error_room_admin_quantity_exceeds_the_upper_limit;
      case LiveError.requestIdConflict: // 100102
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_signal_request_conflict;
      case LiveError.seatLocked: // 100200
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_mic_seat_is_locked;
      case LiveError.seatOccupied: // 100201
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_seat_is_already_occupied;
      case LiveError.alreadyOnTheSeatQueue: // 100202
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_already_on_the_mic_queue;
      case LiveError.alreadyInSeat: // 100203
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_already_on_the_mic;
      case LiveError.notOnTheSeatQueue: // 100204
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_not_on_the_mic_queue;
      case LiveError.allSeatOccupied: // 100205
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_the_seats_are_all_taken;
      case LiveError.userNotInSeat: // 100206
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_not_on_the_mic_seat;
      case LiveError.userAlreadyOnSeat: // 100210
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_user_is_already_on_the_mic_seat;
      case LiveError.seatNotSupportLinkMic: // 100211
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_room_does_not_support_mic_ability;
      case LiveError.emptySeatList: // 100251
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_the_seat_list_is_empty;
      case LiveError.connectionNotExist: // 100400
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_connection_does_not_exist;
      case LiveError.roomInConnection: // 100401
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_room_is_in_connection;
      case LiveError.pendingConnectionRequest: // 100402
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_there_is_a_pending_connection_request;
      case LiveError.roomConnectedInOther: // 100403
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_is_connecting_with_other_rooms;
      case LiveError.connectionOrBattleLimitExceeded: // 100404
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_server_error_has_exceeded_the_limit_in_connection_or_battle;
      case LiveError.creatingConnectionTooFrequent: // 100405
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_creating_connections_too_frequent;
      case LiveError.battleNotExistOrEnded: // 100411
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_battle_does_not_exist_or_has_ended;
      case LiveError.noRoomsInBattleIsValid: // 100412
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_no_rooms_in_the_battle_is_valid;
      case LiveError.creatingBattleTooFrequently: // 100413
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_creating_battles_too_frequently;
      case LiveError.roomNotInBattle: // 100414
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_the_room_is_not_in_the_battle;
      case LiveError.inOtherBattle: // 100415
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_in_other_battle;
      case LiveError.pendingBattleRequest: // 100416
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_there_is_a_pending_battle_request;
      case LiveError.notAllowedCancelBattleForRoomInBattle: // 100419
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_server_error_is_not_allowed_to_cancel_battle_for_room_in_battle;
      case LiveError.battleNotStart: // 100420
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_not_started_yet;
      case LiveError.battleHasEnded: // 100421
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_battle_session_has_ended;
      case LiveError.metadataKeyExceedsLimit: // 100500
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_server_error_metadata_number_of_keys_exceeds_the_limit;
      case LiveError.metadataValueSizeExceedsByteLimit: // 100501
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_server_error_metadata_size_of_value_exceeds_the_limit;
      case LiveError.metadataTotalValueSizeExceedsByteLimit: // 100502
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_metadata_total_size_exceeds_the_limit;
      case LiveError.metadataNoValidKey: // 100503
        return LiveKitLocalizations.of(Global.appContext())?.common_server_error_metadata_no_valid_keys;
      case LiveError.metadataKeySizeExceedsByteLimit: // 100504
        return LiveKitLocalizations.of(Global.appContext())
            ?.common_server_error_metadata_the_size_of_key_exceeds_the_maximum_byte_limit;
      default:
        return '${LiveKitLocalizations.of(Global.appContext())?.common_client_error_failed}, code: $code';
    }
  }
}

enum TIMError {
  success(0),
  failed(-1),
  invalidUserId(7002),
  errSdkCommApiCallFrequencyLimit(7008),
  errSdkBlockedBySensitiveWord(7015),
  errSdkNetDisconnect(9508),
  errSdkNetAllReadyConn(9509),
  errSdkNetConnTimeout(9510),
  errSdkNetConnRefuse(9511),
  errSdkNetNetUnReach(9512),
  errSdkNetWaitInQueueTimeout(9518),
  errSdkNetWaitSendTimeout(9519),
  errSdkNetWaitAckTimeout(9520),
  errSdkNetWaitSendRemainingTimeout(9521),
  errSdkNetPkgSizeLimit(9522),
  errSdkNetWaitSendTimeoutNoNetwork(9523),
  errSdkNetWaitAckTimeoutNoNetwork(9524),
  errSdkNetSendRemainingTimeoutNoNetwork(9525),
  errSvrGroupShutUpDeny(10017);

  final int code;

  const TIMError(this.code);

  static TIMError? fromInt(int code) {
    for (var enumValue in TIMError.values) {
      if (enumValue.code == code) return enumValue;
    }
    return null;
  }
}

extension TIMErrorWithLocalization on TIMError {
  String? get description {
    switch (this) {
      case TIMError.success:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_success;
      case TIMError.invalidUserId:
        return LiveKitLocalizations.of(Global.appContext())?.live_invalid_userId;
      case TIMError.errSdkCommApiCallFrequencyLimit:
        return LiveKitLocalizations.of(Global.appContext())?.common_client_error_freq_limit;
      case TIMError.errSdkBlockedBySensitiveWord:
        return LiveKitLocalizations.of(Global.appContext())?.live_barrage_error_sensitive_word;
      case TIMError.errSdkNetPkgSizeLimit:
        return LiveKitLocalizations.of(Global.appContext())?.live_barrage_error_content_is_long;
      case TIMError.errSdkNetDisconnect:
      case TIMError.errSdkNetWaitAckTimeout:
      case TIMError.errSdkNetAllReadyConn:
      case TIMError.errSdkNetConnTimeout:
      case TIMError.errSdkNetConnRefuse:
      case TIMError.errSdkNetNetUnReach:
      case TIMError.errSdkNetWaitInQueueTimeout:
      case TIMError.errSdkNetWaitSendTimeout:
      case TIMError.errSdkNetWaitSendRemainingTimeout:
      case TIMError.errSdkNetWaitSendTimeoutNoNetwork:
      case TIMError.errSdkNetWaitAckTimeoutNoNetwork:
      case TIMError.errSdkNetSendRemainingTimeoutNoNetwork:
        return LiveKitLocalizations.of(Global.appContext())?.live_barrage_error_network;
      default:
        return '${LiveKitLocalizations.of(Global.appContext())?.common_client_error_failed}, code: $code';
    }
  }
}
