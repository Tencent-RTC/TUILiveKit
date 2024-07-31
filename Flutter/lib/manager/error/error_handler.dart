import 'package:tencent_live_uikit/common/index.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';

class ErrorHandler {
  static const Set<TUIError> interceptToastOnlyPrintLog = {
    TUIError.errFreqLimit,
    TUIError.errRepeatOperation,
    TUIError.errSeatNotSupportLinkMic,
  };

  static void onError(TUIError error) {
    if (error == TUIError.success) {
      return;
    }
    String? message = convertToErrorMessage(error);
    LiveKitLogger.info('ErrorHandler :[error: $error, message: $message]');
    if (!interceptToastOnlyPrintLog.contains(error) && message != null) {
      makeToast(msg: message);
    }
  }

  static void handleMessage(String message) {
    makeToast(msg: message);
  }

  static String? convertToErrorMessage(TUIError error) {
    String? message;
    switch (error) {
      case TUIError.success:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_success;
        break;
      case TUIError.errFreqLimit:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_freqLimit;
        break;
      case TUIError.errRepeatOperation:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_repeat_operation;
        break;
      case TUIError.errSDKAppIDNotFound:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_sdkAppId_notFound;
        break;
      case TUIError.errSdkNotInitialized:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_sdkNotInitialized;
        break;
      case TUIError.errInvalidParameter:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_invalidParameter;
        break;
      case TUIError.errRequirePayment:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_requirePayment;
        break;
      case TUIError.errCameraStartFailed:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_cameraStartFail;
        break;
      case TUIError.errCameraNotAuthorized:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_cameraNotAuthorized;
        break;
      case TUIError.errCameraOccupy:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_cameraOccupied;
        break;
      case TUIError.errCameraDeviceEmpty:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_cameraDeviceEmpty;
        break;
      case TUIError.errMicrophoneStartFailed:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_microphoneStartFail;
        break;
      case TUIError.errMicrophoneNotAuthorized:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_microphoneNotAuthorized;
        break;
      case TUIError.errMicrophoneOccupy:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_microphoneOccupied;
        break;
      case TUIError.errMicrophoneDeviceEmpty:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_microphoneDeviceEmpty;
        break;
      case TUIError.errGetScreenSharingTargetFailed:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_getScreenSharingTargetFailed;
        break;
      case TUIError.errStartScreenSharingFailed:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_startScreenSharingFailed;
        break;
      case TUIError.errRoomIdNotExist:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_roomId_notExist;
        break;
      case TUIError.errOperationInvalidBeforeEnterRoom:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_operation_invalid_beforeEnterRoom;
        break;
      case TUIError.errExitNotSupportedForRoomOwner:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_exitNotSupported_forRoomOwner;
        break;
      case TUIError.errOperationNotSupportedInCurrentRoomType:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_operation_notSupported_inCurrentRoomType;
        break;
      case TUIError.errRoomIdInvalid:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_roomId_invalid;
        break;
      case TUIError.errRoomIdOccupied:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_roomId_occupied;
        break;
      case TUIError.errRoomNameInvalid:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_roomName_invalid;
        break;
      case TUIError.errAlreadyInOtherRoom:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_already_in_OtherRoom;
        break;
      case TUIError.errUserNotExist:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_userNotExist;
        break;
      case TUIError.errUserNotEntered:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_userNotEntered;
        break;
      case TUIError.errUserNeedOwnerPermission:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_user_need_OwnerPermission;
        break;
      case TUIError.errUserNeedAdminPermission:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_user_need_AdminPermission;
        break;
      case TUIError.errRequestNoPermission:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_request_noPermission;
        break;
      case TUIError.errRequestIdInvalid:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_requestId_invalid;
        break;
      case TUIError.errRequestIdRepeat:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_repeat_requestId;
        break;
      case TUIError.errRequestIdConflict:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_conflict_requestId;
        break;
      case TUIError.errMaxSeatCountLimit:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_max_seat_count_limit;
        break;
      case TUIError.errAlreadyInSeat:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_already_in_seat;
        break;
      case TUIError.errSeatOccupied:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_seat_occupied;
        break;
      case TUIError.errSeatLocked:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_seat_locked;
        break;
      case TUIError.errSeatIndexNotExist:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_seat_index_not_exist;
        break;
      case TUIError.errUserNotInSeat:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_user_not_in_seat;
        break;
      case TUIError.errAllSeatOccupied:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_all_seat_occupied;
        break;
      case TUIError.errSeatNotSupportLinkMic:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_seat_not_support_link_mic;
        break;
      case TUIError.errOpenMicrophoneNeedSeatUnlock:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_open_microphone_need_seat_unlock;
        break;
      case TUIError.errOpenMicrophoneNeedPermissionFromAdmin:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_open_microphone_need_permission_from_admin;
        break;
      case TUIError.errOpenCameraNeedSeatUnlock:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_open_camera_need_seat_unlock;
        break;
      case TUIError.errOpenCameraNeedPermissionFromAdmin:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_open_camera_need_permission_from_admin;
        break;
      case TUIError.errOpenScreenShareNeedSeatUnlock:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_open_screen_share_need_seat_unlock;
        break;
      case TUIError.errOpenScreenShareNeedPermissionFromAdmin:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_open_screen_share_need_permission_from_admin;
        break;
      case TUIError.errSendMessageDisabledForAll:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_send_message_disabled_for_all;
        break;
      case TUIError.errSendMessageDisabledForCurrent:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_send_message_disabled_for_current;
        break;
      default:
        message = LiveKitLocalizations.of(Global.appContext())?.livekit_error_freqLimit;
        break;
    }
    return message;
  }
}