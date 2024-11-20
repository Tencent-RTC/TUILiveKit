package com.trtc.uikit.livekit.livestream.manager.error;

import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.Error.FREQ_LIMIT;
import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.Error.REPEAT_OPERATION;
import static com.tencent.cloud.tuikit.engine.common.TUICommonDefine.Error.SEAT_NOT_SUPPORT_LINK_MIC;

import android.content.Context;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.manager.api.LiveStreamLog;

import java.util.Arrays;
import java.util.HashSet;

public class ErrorHandler {

    private static final HashSet<TUICommonDefine.Error> INTERCEPT_TOAST_ONLY_PRINT_LOG = new HashSet<>(
            Arrays.asList(FREQ_LIMIT, REPEAT_OPERATION, SEAT_NOT_SUPPORT_LINK_MIC));

    public static void onError(TUICommonDefine.Error error) {
        if (error == TUICommonDefine.Error.SUCCESS) {
            return;
        }
        String message = convertToErrorMessage(error);
        LiveStreamLog.info("ErrorHandler :[error:" + error + ",message:" + message + "]");
        if (!INTERCEPT_TOAST_ONLY_PRINT_LOG.contains(error)) {
            ToastUtil.toastShortMessage(message);
        }
    }

    public static void handleMessage(String message) {
        ToastUtil.toastShortMessage(message);
    }

    private static String convertToErrorMessage(TUICommonDefine.Error error) {
        int stringResId;
        switch (error) {
            case SUCCESS:
                stringResId = R.string.livekit_error_success;
                break;
            case FREQ_LIMIT:
                stringResId = R.string.livekit_error_freqLimit;
                break;
            case REPEAT_OPERATION:
                stringResId = R.string.livekit_error_repeat_operation;
                break;
            case SDKAPPID_NOT_FOUND:
                stringResId = R.string.livekit_error_sdkAppId_notFound;
                break;
            case INVALID_PARAMETER:
                stringResId = R.string.livekit_error_invalidParameter;
                break;
            case SDK_NOT_INITIALIZED:
                stringResId = R.string.livekit_error_sdkNotInitialized;
                break;
            case PERMISSION_DENIED:
                stringResId = R.string.livekit_error_permissionDenied;
                break;
            case REQUIRE_PAYMENT:
                stringResId = R.string.livekit_error_requirePayment;
                break;
            case CAMERA_START_FAIL:
                stringResId = R.string.livekit_error_cameraStartFail;
                break;
            case CAMERA_NOT_AUTHORIZED:
                stringResId = R.string.livekit_error_cameraNotAuthorized;
                break;
            case CAMERA_OCCUPIED:
                stringResId = R.string.livekit_error_cameraOccupied;
                break;
            case CAMERA_DEVICE_EMPTY:
                stringResId = R.string.livekit_error_cameraDeviceEmpty;
                break;
            case MICROPHONE_START_FAIL:
                stringResId = R.string.livekit_error_microphoneStartFail;
                break;
            case MICROPHONE_NOT_AUTHORIZED:
                stringResId = R.string.livekit_error_microphoneNotAuthorized;
                break;
            case MICROPHONE_OCCUPIED:
                stringResId = R.string.livekit_error_microphoneOccupied;
                break;
            case MICROPHONE_DEVICE_EMPTY:
                stringResId = R.string.livekit_error_microphoneDeviceEmpty;
                break;
            case GET_SCREEN_SHARING_TARGET_FAILED:
                stringResId = R.string.livekit_error_getScreenSharingTargetFailed;
                break;
            case START_SCREEN_SHARING_FAILED:
                stringResId = R.string.livekit_error_startScreenSharingFailed;
                break;
            case ROOM_ID_NOT_EXIST:
                stringResId = R.string.livekit_error_roomId_notExist;
                break;
            case OPERATION_INVALID_BEFORE_ENTER_ROOM:
                stringResId = R.string.livekit_error_operation_invalid_beforeEnterRoom;
                break;
            case EXIT_NOT_SUPPORTED_FOR_ROOM_OWNER:
                stringResId = R.string.livekit_error_exitNotSupported_forRoomOwner;
                break;
            case OPERATION_NOT_SUPPORTED_IN_CURRENT_ROOM_TYPE:
                stringResId = R.string.livekit_error_operation_notSupported_inCurrentRoomType;
                break;
            case ROOM_ID_INVALID:
                stringResId = R.string.livekit_error_roomId_invalid;
                break;
            case ROOM_ID_OCCUPIED:
                stringResId = R.string.livekit_error_roomId_occupied;
                break;
            case ROOM_NAME_INVALID:
                stringResId = R.string.livekit_error_roomName_invalid;
                break;
            case ALREADY_IN_OTHER_ROOM:
                stringResId = R.string.livekit_error_already_in_OtherRoom;
                break;
            case USER_NOT_EXIST:
                stringResId = R.string.livekit_error_userNotExist;
                break;
            case USER_NOT_ENTERED:
                stringResId = R.string.livekit_error_userNotEntered;
                break;
            case NEED_OWNER_PERMISSION:
                stringResId = R.string.livekit_error_user_need_OwnerPermission;
                break;
            case NEED_ADMIN_PERMISSION:
                stringResId = R.string.livekit_error_user_need_AdminPermission;
                break;
            case REQUEST_NO_PERMISSION:
                stringResId = R.string.livekit_error_request_noPermission;
                break;
            case REQUEST_ID_INVALID:
                stringResId = R.string.livekit_error_requestId_invalid;
                break;
            case REQUEST_ID_REPEAT:
                stringResId = R.string.livekit_error_repeat_requestId;
                break;
            case REQUEST_ID_CONFLICT:
                stringResId = R.string.livekit_error_conflict_requestId;
                break;
            case MAX_SEAT_COUNT_LIMIT:
                stringResId = R.string.livekit_error_max_seat_count_limit;
                break;
            case ALREADY_IN_SEAT:
                stringResId = R.string.livekit_error_already_in_seat;
                break;
            case SEAT_OCCUPIED:
                stringResId = R.string.livekit_error_seat_occupied;
                break;
            case SEAT_LOCKED:
                stringResId = R.string.livekit_error_seat_locked;
                break;
            case SEAT_INDEX_NOT_EXIST:
                stringResId = R.string.livekit_error_seat_index_not_exist;
                break;
            case USER_NOT_IN_SEAT:
                stringResId = R.string.livekit_error_user_not_in_seat;
                break;
            case ALL_SEAT_OCCUPIED:
                stringResId = R.string.livekit_error_all_seat_occupied;
                break;
            case SEAT_NOT_SUPPORT_LINK_MIC:
                stringResId = R.string.livekit_error_seat_not_support_link_mic;
                break;
            case OPEN_MICROPHONE_NEED_SEAT_UNLOCK:
                stringResId = R.string.livekit_error_open_microphone_need_seat_unlock;
                break;
            case OPEN_MICROPHONE_NEED_PERMISSION_FROM_ADMIN:
                stringResId = R.string.livekit_error_open_microphone_need_permission_from_admin;
                break;
            case OPEN_CAMERA_NEED_SEAT_UNLOCK:
                stringResId = R.string.livekit_error_open_camera_need_seat_unlock;
                break;
            case OPEN_CAMERA_NEED_PERMISSION_FROM_ADMIN:
                stringResId = R.string.livekit_error_open_camera_need_permission_from_admin;
                break;
            case OPEN_SCREEN_SHARE_NEED_SEAT_UNLOCK:
                stringResId = R.string.livekit_error_open_screen_share_need_seat_unlock;
                break;
            case OPEN_SCREEN_SHARE_NEED_PERMISSION_FROM_ADMIN:
                stringResId = R.string.livekit_error_open_screen_share_need_permission_from_admin;
                break;
            case SEND_MESSAGE_DISABLED_FOR_ALL:
                stringResId = R.string.livekit_error_send_message_disabled_for_all;
                break;
            case SEND_MESSAGE_DISABLED_FOR_CURRENT:
                stringResId = R.string.livekit_error_send_message_disabled_for_current;
                break;
            case ROOM_CONNECTED_IN_OTHER:
                stringResId = R.string.livekit_error_room_connected_in_other;
                break;
            default:
                stringResId = R.string.livekit_error_failed;
                break;

        }
        Context context = TUIConfig.getAppContext();
        if (context == null) {
            return "";
        }
        return context.getString(stringResId);
    }
}
