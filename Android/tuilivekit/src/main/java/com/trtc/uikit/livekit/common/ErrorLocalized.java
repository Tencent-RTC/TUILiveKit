package com.trtc.uikit.livekit.common;

import android.content.Context;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.util.ToastUtil;
import com.trtc.uikit.livekit.R;

import java.util.Arrays;
import java.util.HashSet;

public class ErrorLocalized {
    public static final int LIVE_CLIENT_ERROR_SUCCESS                                      = 0;
    public static final int LIVE_CLIENT_ERROR_FREQ_LIMIT                                   = -2;
    public static final int LIVE_CLIENT_ERROR_REPEAT_OPERATION                             = -3;
    public static final int LIVE_CLIENT_ERROR_ROOM_MISMATCH                                = -4;
    public static final int LIVE_CLIENT_ERROR_SDK_APP_ID_NOT_FOUND                         = -1000;
    public static final int LIVE_CLIENT_ERROR_INVALID_PARAMETER                            = -1001;
    public static final int LIVE_CLIENT_ERROR_SDK_NOT_INITIALIZED                          = -1002;
    public static final int LIVE_CLIENT_ERROR_PERMISSION_DENIED                            = -1003;
    public static final int LIVE_CLIENT_ERROR_REQUIRE_PAYMENT                              = -1004;
    public static final int LIVE_CLIENT_ERROR_CAMERA_START_FAIL                            = -1100;
    public static final int LIVE_CLIENT_ERROR_CAMERA_NOT_AUTHORIZED                        = -1101;
    public static final int LIVE_CLIENT_ERROR_CAMERA_OCCUPIED                              = -1102;
    public static final int LIVE_CLIENT_ERROR_CAMERA_DEVICE_EMPTY                          = -1103;
    public static final int LIVE_CLIENT_ERROR_MICROPHONE_START_FAIL                        = -1104;
    public static final int LIVE_CLIENT_ERROR_MICROPHONE_NOT_AUTHORIZED                    = -1105;
    public static final int LIVE_CLIENT_ERROR_MICROPHONE_OCCUPIED                          = -1106;
    public static final int LIVE_CLIENT_ERROR_MICROPHONE_DEVICE_EMPTY                      = -1107;
    public static final int LIVE_CLIENT_ERROR_GET_SCREEN_SHARING_TARGET_FAILED             = -1108;
    public static final int LIVE_CLIENT_ERROR_START_SCREEN_SHARING_FAILED                  = -1109;
    public static final int LIVE_CLIENT_ERROR_OPERATION_INVALID_BEFORE_ENTER_ROOM          = -2101;
    public static final int LIVE_CLIENT_ERROR_EXIT_NOT_SUPPORTED_FOR_ROOM_OWNER            = -2102;
    public static final int LIVE_CLIENT_ERROR_OPERATION_NOT_SUPPORTED_IN_CURRENT_ROOM_TYPE = -2103;
    public static final int LIVE_CLIENT_ERROR_ROOM_ID_INVALID                              = -2105;
    public static final int LIVE_CLIENT_ERROR_ROOM_NAME_INVALID                            = -2107;
    public static final int LIVE_CLIENT_ERROR_ALREADY_IN_OTHER_ROOM                        = -2108;
    public static final int LIVE_CLIENT_ERROR_USER_NOT_EXIST                               = -2200;
    public static final int LIVE_CLIENT_ERROR_USER_NEED_OWNER_PERMISSION                   = -2300;
    public static final int LIVE_CLIENT_ERROR_USER_NEED_ADMIN_PERMISSION                   = -2301;
    public static final int LIVE_CLIENT_ERROR_REQUEST_NO_PERMISSION                        = -2310;
    public static final int LIVE_CLIENT_ERROR_REQUEST_ID_INVALID                           = -2311;
    public static final int LIVE_CLIENT_ERROR_REQUEST_ID_REPEAT                            = -2312;
    public static final int LIVE_CLIENT_ERROR_MAX_SEAT_COUNT_LIMIT                         = -2340;
    public static final int LIVE_CLIENT_ERROR_SEAT_INDEX_NOT_EXIST                         = -2344;
    public static final int LIVE_CLIENT_ERROR_OPEN_MICROPHONE_NEED_SEAT_UNLOCK             = -2360;
    public static final int LIVE_CLIENT_ERROR_OPEN_MICROPHONE_NEED_PERMISSION_FROM_ADMIN   = -2361;
    public static final int LIVE_CLIENT_ERROR_OPEN_CAMERA_NEED_SEAT_UNLOCK                 = -2370;
    public static final int LIVE_CLIENT_ERROR_OPEN_CAMERA_NEED_PERMISSION_FROM_ADMIN       = -2371;
    public static final int LIVE_CLIENT_ERROR_OPEN_SCREEN_SHARE_NEED_SEAT_UNLOCK           = -2372;
    public static final int LIVE_CLIENT_ERROR_OPEN_SCREEN_SHARE_NEED_PERMISSION_FROM_ADMIN = -2373;
    public static final int LIVE_CLIENT_ERROR_SEND_MESSAGE_DISABLED_FOR_ALL                = -2380;
    public static final int LIVE_CLIENT_ERROR_SEND_MESSAGE_DISABLED_FOR_CURRENT            = -2381;
    public static final int LIVE_CLIENT_ERROR_ROOM_NOT_SUPPORT_PRELOADING                  = -4001;
    public static final int LIVE_CLIENT_ERROR_CALL_IN_PROGRESS                             = -6001;

    public static final int LIVE_SERVER_ERROR_SYSTEM_INTERNAL_ERROR                                   = 100001;
    public static final int LIVE_SERVER_ERROR_PARAM_ILLEGAL                                           = 100002;
    public static final int LIVE_SERVER_ERROR_ROOM_ID_EXISTS                                          = 100003;
    public static final int LIVE_SERVER_ERROR_ROOM_DOES_NOT_EXIST                                     = 100004;
    public static final int LIVE_SERVER_ERROR_NOT_A_ROOM_MEMBER                                       = 100005;
    public static final int LIVE_SERVER_ERROR_INSUFFICIENT_OPERATION_PERMISSIONS                      = 100006;
    public static final int LIVE_SERVER_ERROR_NO_PAYMENT_INFORMATION                                  = 100007;
    public static final int LIVE_SERVER_ERROR_ROOM_IS_FULL                                            = 100008;
    public static final int LIVE_SERVER_ERROR_TAG_QUANTITY_EXCEEDS_UPPER_LIMIT                        = 100009;
    public static final int LIVE_SERVER_ERROR_ROOM_ID_HAS_BEEN_USED                                   = 100010;
    public static final int LIVE_SERVER_ERROR_ROOM_ID_HAS_BEEN_OCCUPIED_BY_CHAT                       = 100011;
    public static final int LIVE_SERVER_ERROR_CREATING_ROOMS_EXCEEDS_THE_FREQUENCY_LIMIT              = 100012;
    public static final int LIVE_SERVER_ERROR_EXCEEDS_THE_UPPER_LIMIT                                 = 100013;
    public static final int LIVE_SERVER_ERROR_INVALID_ROOM_TYPE                                       = 100015;
    public static final int LIVE_SERVER_ERROR_THIS_MEMBER_HAS_BEEN_BANNED                             = 100016;
    public static final int LIVE_SERVER_ERROR_THIS_MEMBER_HAS_BEEN_MUTED                              = 100017;
    public static final int LIVE_SERVER_ERROR_REQUIRES_PASSWORD                                       = 100018;
    public static final int LIVE_SERVER_ERROR_ROOM_ENTRY_PASSWORD_ERROR                               = 100019;
    public static final int LIVE_SERVER_ERROR_ROOM_ADMIN_QUANTITY_EXCEEDS_THE_UPPER_LIMIT             = 100020;
    public static final int LIVE_SERVER_ERROR_SIGNAL_REQUEST_CONFLICT                                 = 100102;
    public static final int LIVE_SERVER_ERROR_MIC_SEAT_IS_LOCKED                                      = 100200;
    public static final int LIVE_SERVER_ERROR_SEAT_IS_ALREADY_OCCUPIED                                = 100201;
    public static final int LIVE_SERVER_ERROR_ALREADY_ON_THE_MIC_QUEUE                                = 100202;
    public static final int LIVE_SERVER_ERROR_ALREADY_ON_THE_MIC                                      = 100203;
    public static final int LIVE_SERVER_ERROR_NOT_ON_THE_MIC_QUEUE                                    = 100204;
    public static final int LIVE_SERVER_ERROR_THE_SEATS_ARE_ALL_TAKEN                                 = 100205;
    public static final int LIVE_SERVER_ERROR_NOT_ON_THE_MIC_SEAT                                     = 100206;
    public static final int LIVE_SERVER_ERROR_USER_IS_ALREADY_ON_THE_MIC_SEAT                         = 100210;
    public static final int LIVE_SERVER_ERROR_ROOM_DOES_NOT_SUPPORT_MIC_ABILITY                       = 100211;
    public static final int LIVE_SERVER_ERROR_THE_SEAT_LIST_IS_EMPTY                                  = 100251;
    public static final int LIVE_SERVER_ERROR_CONNECTION_DOES_NOT_EXIST                               = 100400;
    public static final int LIVE_SERVER_ERROR_ROOM_IS_IN_CONNECTION                                   = 100401;
    public static final int LIVE_SERVER_ERROR_THERE_IS_A_PENDING_CONNECTION_REQUEST                   = 100402;
    public static final int LIVE_SERVER_ERROR_IS_CONNECTING_WITH_OTHER_ROOMS                          = 100403;
    public static final int LIVE_SERVER_ERROR_HAS_EXCEEDED_THE_LIMIT_IN_CONNECTION_OR_BATTLE          = 100404;
    public static final int LIVE_SERVER_ERROR_CREATING_CONNECTIONS_TOO_FREQUENT                       = 100405;
    public static final int LIVE_SERVER_ERROR_BATTLE_DOES_NOT_EXIST_OR_HAS_ENDED                      = 100411;
    public static final int LIVE_SERVER_ERROR_NO_ROOMS_IN_THE_BATTLE_IS_VALID                         = 100412;
    public static final int LIVE_SERVER_ERROR_CREATING_BATTLES_TOO_FREQUENTLY                         = 100413;
    public static final int LIVE_SERVER_ERROR_THE_ROOM_IS_NOT_IN_THE_BATTLE                           = 100414;
    public static final int LIVE_SERVER_ERROR_IN_OTHER_BATTLE                                         = 100415;
    public static final int LIVE_SERVER_ERROR_THERE_IS_A_PENDING_BATTLE_REQUEST                       = 100416;
    public static final int LIVE_SERVER_ERROR_IS_NOT_ALLOWED_TO_CANCEL_BATTLE_FOR_ROOM_IN_BATTLE      = 100419;
    public static final int LIVE_SERVER_ERROR_NOT_STARTED_YET                                         = 100420;
    public static final int LIVE_SERVER_ERROR_BATTLE_SESSION_HAS_ENDED                                = 100421;
    public static final int LIVE_SERVER_ERROR_METADATA_NUMBER_OF_KEYS_EXCEEDS_THE_LIMIT               = 100500;
    public static final int LIVE_SERVER_ERROR_METADATA_SIZE_OF_VALUE_EXCEEDS_THE_LIMIT                = 100501;
    public static final int LIVE_SERVER_ERROR_METADATA_TOTAL_SIZE_EXCEEDS_THE_LIMIT                   = 100502;
    public static final int LIVE_SERVER_ERROR_METADATA_NO_VALID_KEYS                                  = 100503;
    public static final int LIVE_SERVER_ERROR_METADATA_THE_SIZE_OF_KEY_EXCEEDS_THE_MAXIMUM_BYTE_LIMIT = 100504;

    private static final HashSet<Integer> INTERCEPT_TOAST_ONLY_PRINT_LOG =
            new HashSet<>(Arrays.asList(LIVE_CLIENT_ERROR_ROOM_MISMATCH));

    private static final LiveKitLogger LOGGER = LiveKitLogger.getCommonLogger("ErrorLocalized");

    public static void onError(TUICommonDefine.Error error) {
        if (error == TUICommonDefine.Error.SUCCESS) {
            return;
        }
        String message = convertToErrorMessage(error);
        LOGGER.info("[error:" + error + ",value:" + error.getValue() + ",message:" + message + "]");
        if (!INTERCEPT_TOAST_ONLY_PRINT_LOG.contains(error.getValue())) {
            ToastUtil.toastShortMessage(message);
        }
    }

    private static String convertToErrorMessage(TUICommonDefine.Error error) {
        String error_message = "";
        Context context = TUIConfig.getAppContext();
        if (context == null) {
            return "";
        }
        switch (error.getValue()) {
            case LIVE_CLIENT_ERROR_SUCCESS:
                error_message = context.getString(R.string.common_client_error_success);
                break;
            case LIVE_CLIENT_ERROR_FREQ_LIMIT:
                error_message = context.getString(R.string.common_client_error_freq_limit);
                break;
            case LIVE_CLIENT_ERROR_REPEAT_OPERATION:
                error_message = context.getString(R.string.common_client_error_repeat_operation);
                break;
            case LIVE_CLIENT_ERROR_SDK_APP_ID_NOT_FOUND:
                error_message = context.getString(R.string.common_client_error_sdk_app_id_not_found);
                break;
            case LIVE_CLIENT_ERROR_INVALID_PARAMETER:
                error_message = context.getString(R.string.common_client_error_invalid_parameter);
                break;
            case LIVE_CLIENT_ERROR_SDK_NOT_INITIALIZED:
                error_message = context.getString(R.string.common_client_error_sdk_not_initialized);
                break;
            case LIVE_CLIENT_ERROR_PERMISSION_DENIED:
                error_message = context.getString(R.string.common_client_error_permission_denied);
                break;
            case LIVE_CLIENT_ERROR_REQUIRE_PAYMENT:
                error_message = context.getString(R.string.common_client_error_require_payment);
                break;
            case LIVE_CLIENT_ERROR_CAMERA_START_FAIL:
                error_message = context.getString(R.string.common_client_error_camera_start_fail);
                break;
            case LIVE_CLIENT_ERROR_CAMERA_NOT_AUTHORIZED:
                error_message = context.getString(R.string.common_client_error_camera_not_authorized);
                break;
            case LIVE_CLIENT_ERROR_CAMERA_OCCUPIED:
                error_message = context.getString(R.string.common_client_error_camera_occupied);
                break;
            case LIVE_CLIENT_ERROR_CAMERA_DEVICE_EMPTY:
                error_message = context.getString(R.string.common_client_error_camera_device_empty);
                break;
            case LIVE_CLIENT_ERROR_MICROPHONE_START_FAIL:
                error_message = context.getString(R.string.common_client_error_microphone_start_fail);
                break;
            case LIVE_CLIENT_ERROR_MICROPHONE_NOT_AUTHORIZED:
                error_message = context.getString(R.string.common_client_error_microphone_not_authorized);
                break;
            case LIVE_CLIENT_ERROR_MICROPHONE_OCCUPIED:
                error_message = context.getString(R.string.common_client_error_microphone_occupied);
                break;
            case LIVE_CLIENT_ERROR_MICROPHONE_DEVICE_EMPTY:
                error_message = context.getString(R.string.common_client_error_microphone_device_empty);
                break;
            case LIVE_CLIENT_ERROR_GET_SCREEN_SHARING_TARGET_FAILED:
                error_message = context.getString(R.string.common_client_error_get_screen_sharing_target_failed);
                break;
            case LIVE_CLIENT_ERROR_START_SCREEN_SHARING_FAILED:
                error_message = context.getString(R.string.common_client_error_start_screen_sharing_failed);
                break;
            case LIVE_CLIENT_ERROR_OPERATION_INVALID_BEFORE_ENTER_ROOM:
                error_message = context.getString(R.string.common_client_error_operation_invalid_before_enter_room);
                break;
            case LIVE_CLIENT_ERROR_EXIT_NOT_SUPPORTED_FOR_ROOM_OWNER:
                error_message = context.getString(R.string.common_client_error_exit_not_supported_for_room_owner);
                break;
            case LIVE_CLIENT_ERROR_OPERATION_NOT_SUPPORTED_IN_CURRENT_ROOM_TYPE:
                error_message =
                        context.getString(R.string.common_client_error_operation_not_supported_in_current_room_type);
                break;
            case LIVE_CLIENT_ERROR_ROOM_ID_INVALID:
                error_message = context.getString(R.string.common_client_error_room_id_invalid);
                break;
            case LIVE_CLIENT_ERROR_ROOM_NAME_INVALID:
                error_message = context.getString(R.string.common_client_error_room_name_invalid);
                break;
            case LIVE_CLIENT_ERROR_ALREADY_IN_OTHER_ROOM:
                error_message = context.getString(R.string.common_client_error_already_in_other_room);
                break;
            case LIVE_CLIENT_ERROR_USER_NOT_EXIST:
                error_message = context.getString(R.string.common_client_error_user_not_exist);
                break;
            case LIVE_CLIENT_ERROR_USER_NEED_OWNER_PERMISSION:
                error_message = context.getString(R.string.common_client_error_user_need_owner_permission);
                break;
            case LIVE_CLIENT_ERROR_USER_NEED_ADMIN_PERMISSION:
                error_message = context.getString(R.string.common_client_error_user_need_admin_permission);
                break;
            case LIVE_CLIENT_ERROR_REQUEST_NO_PERMISSION:
                error_message = context.getString(R.string.common_client_error_request_no_permission);
                break;
            case LIVE_CLIENT_ERROR_REQUEST_ID_INVALID:
                error_message = context.getString(R.string.common_client_error_request_id_invalid);
                break;
            case LIVE_CLIENT_ERROR_REQUEST_ID_REPEAT:
                error_message = context.getString(R.string.common_client_error_request_id_repeat);
                break;
            case LIVE_CLIENT_ERROR_MAX_SEAT_COUNT_LIMIT:
                error_message = context.getString(R.string.common_client_error_max_seat_count_limit);
                break;
            case LIVE_CLIENT_ERROR_SEAT_INDEX_NOT_EXIST:
                error_message = context.getString(R.string.common_client_error_seat_index_not_exist);
                break;
            case LIVE_CLIENT_ERROR_OPEN_MICROPHONE_NEED_SEAT_UNLOCK:
                error_message = context.getString(R.string.common_client_error_open_microphone_need_seat_unlock);
                break;
            case LIVE_CLIENT_ERROR_OPEN_MICROPHONE_NEED_PERMISSION_FROM_ADMIN:
                error_message =
                        context.getString(R.string.common_client_error_open_microphone_need_permission_from_admin);
                break;
            case LIVE_CLIENT_ERROR_OPEN_CAMERA_NEED_SEAT_UNLOCK:
                error_message = context.getString(R.string.common_client_error_open_camera_need_seat_unlock);
                break;
            case LIVE_CLIENT_ERROR_OPEN_CAMERA_NEED_PERMISSION_FROM_ADMIN:
                error_message = context.getString(R.string.common_client_error_open_camera_need_permission_from_admin);
                break;
            case LIVE_CLIENT_ERROR_OPEN_SCREEN_SHARE_NEED_SEAT_UNLOCK:
                error_message = context.getString(R.string.common_client_error_open_screen_share_need_seat_unlock);
                break;
            case LIVE_CLIENT_ERROR_OPEN_SCREEN_SHARE_NEED_PERMISSION_FROM_ADMIN:
                error_message =
                        context.getString(R.string.common_client_error_open_screen_share_need_permission_from_admin);
                break;
            case LIVE_CLIENT_ERROR_SEND_MESSAGE_DISABLED_FOR_ALL:
                error_message = context.getString(R.string.common_client_error_send_message_disabled_for_all);
                break;
            case LIVE_CLIENT_ERROR_SEND_MESSAGE_DISABLED_FOR_CURRENT:
                error_message = context.getString(R.string.common_client_error_send_message_disabled_for_current);
                break;
            case LIVE_CLIENT_ERROR_ROOM_NOT_SUPPORT_PRELOADING:
                error_message = context.getString(R.string.common_client_error_room_not_support_preloading);
                break;
            case LIVE_CLIENT_ERROR_CALL_IN_PROGRESS:
                error_message = context.getString(R.string.common_server_error_call_in_progress);
                break;
            case LIVE_SERVER_ERROR_SYSTEM_INTERNAL_ERROR:
                error_message = context.getString(R.string.common_server_error_system_internal_error);
                break;
            case LIVE_SERVER_ERROR_PARAM_ILLEGAL:
                error_message = context.getString(R.string.common_server_error_param_illegal);
                break;
            case LIVE_SERVER_ERROR_ROOM_ID_EXISTS:
                error_message = context.getString(R.string.common_server_error_room_id_exists);
                break;
            case LIVE_SERVER_ERROR_ROOM_DOES_NOT_EXIST:
                error_message = context.getString(R.string.common_server_error_room_does_not_exist);
                break;
            case LIVE_SERVER_ERROR_NOT_A_ROOM_MEMBER:
                error_message = context.getString(R.string.common_server_error_not_a_room_member);
                break;
            case LIVE_SERVER_ERROR_INSUFFICIENT_OPERATION_PERMISSIONS:
                error_message = context.getString(R.string.common_server_error_insufficient_operation_permissions);
                break;
            case LIVE_SERVER_ERROR_NO_PAYMENT_INFORMATION:
                error_message = context.getString(R.string.common_server_error_no_payment_information);
                break;
            case LIVE_SERVER_ERROR_ROOM_IS_FULL:
                error_message = context.getString(R.string.common_server_error_room_is_full);
                break;
            case LIVE_SERVER_ERROR_TAG_QUANTITY_EXCEEDS_UPPER_LIMIT:
                error_message = context.getString(R.string.common_server_error_tag_quantity_exceeds_upper_limit);
                break;
            case LIVE_SERVER_ERROR_ROOM_ID_HAS_BEEN_USED:
                error_message = context.getString(R.string.common_server_error_room_id_has_been_used);
                break;
            case LIVE_SERVER_ERROR_ROOM_ID_HAS_BEEN_OCCUPIED_BY_CHAT:
                error_message = context.getString(R.string.common_server_error_room_id_has_been_occupied_by_chat);
                break;
            case LIVE_SERVER_ERROR_CREATING_ROOMS_EXCEEDS_THE_FREQUENCY_LIMIT:
                error_message =
                        context.getString(R.string.common_server_error_creating_rooms_exceeds_the_frequency_limit);
                break;
            case LIVE_SERVER_ERROR_EXCEEDS_THE_UPPER_LIMIT:
                error_message = context.getString(R.string.common_server_error_exceeds_the_upper_limit);
                break;
            case LIVE_SERVER_ERROR_INVALID_ROOM_TYPE:
                error_message = context.getString(R.string.common_server_error_invalid_room_type);
                break;
            case LIVE_SERVER_ERROR_THIS_MEMBER_HAS_BEEN_BANNED:
                error_message = context.getString(R.string.common_server_error_this_member_has_been_banned);
                break;
            case LIVE_SERVER_ERROR_THIS_MEMBER_HAS_BEEN_MUTED:
                error_message = context.getString(R.string.common_server_error_this_member_has_been_muted);
                break;
            case LIVE_SERVER_ERROR_REQUIRES_PASSWORD:
                error_message = context.getString(R.string.common_server_error_requires_password);
                break;
            case LIVE_SERVER_ERROR_ROOM_ENTRY_PASSWORD_ERROR:
                error_message = context.getString(R.string.common_server_error_room_entry_password_error);
                break;
            case LIVE_SERVER_ERROR_ROOM_ADMIN_QUANTITY_EXCEEDS_THE_UPPER_LIMIT:
                error_message =
                        context.getString(R.string.common_server_error_room_admin_quantity_exceeds_the_upper_limit);
                break;
            case LIVE_SERVER_ERROR_SIGNAL_REQUEST_CONFLICT:
                error_message = context.getString(R.string.common_server_error_signal_request_conflict);
                break;
            case LIVE_SERVER_ERROR_MIC_SEAT_IS_LOCKED:
                error_message = context.getString(R.string.common_server_error_mic_seat_is_locked);
                break;
            case LIVE_SERVER_ERROR_SEAT_IS_ALREADY_OCCUPIED:
                error_message = context.getString(R.string.common_server_error_seat_is_already_occupied);
                break;
            case LIVE_SERVER_ERROR_ALREADY_ON_THE_MIC_QUEUE:
                error_message = context.getString(R.string.common_server_error_already_on_the_mic_queue);
                break;
            case LIVE_SERVER_ERROR_ALREADY_ON_THE_MIC:
                error_message = context.getString(R.string.common_server_error_already_on_the_mic);
                break;
            case LIVE_SERVER_ERROR_NOT_ON_THE_MIC_QUEUE:
                error_message = context.getString(R.string.common_server_error_not_on_the_mic_queue);
                break;
            case LIVE_SERVER_ERROR_THE_SEATS_ARE_ALL_TAKEN:
                error_message = context.getString(R.string.common_server_error_the_seats_are_all_taken);
                break;
            case LIVE_SERVER_ERROR_NOT_ON_THE_MIC_SEAT:
                error_message = context.getString(R.string.common_server_error_not_on_the_mic_seat);
                break;
            case LIVE_SERVER_ERROR_USER_IS_ALREADY_ON_THE_MIC_SEAT:
                error_message = context.getString(R.string.common_server_error_user_is_already_on_the_mic_seat);
                break;
            case LIVE_SERVER_ERROR_ROOM_DOES_NOT_SUPPORT_MIC_ABILITY:
                error_message = context.getString(R.string.common_server_error_room_does_not_support_mic_ability);
                break;
            case LIVE_SERVER_ERROR_THE_SEAT_LIST_IS_EMPTY:
                error_message = context.getString(R.string.common_server_error_the_seat_list_is_empty);
                break;
            case LIVE_SERVER_ERROR_CONNECTION_DOES_NOT_EXIST:
                error_message = context.getString(R.string.common_server_error_connection_does_not_exist);
                break;
            case LIVE_SERVER_ERROR_ROOM_IS_IN_CONNECTION:
                error_message = context.getString(R.string.common_server_error_room_is_in_connection);
                break;
            case LIVE_SERVER_ERROR_THERE_IS_A_PENDING_CONNECTION_REQUEST:
                error_message = context.getString(R.string.common_server_error_there_is_a_pending_connection_request);
                break;
            case LIVE_SERVER_ERROR_IS_CONNECTING_WITH_OTHER_ROOMS:
                error_message = context.getString(R.string.common_server_error_is_connecting_with_other_rooms);
                break;
            case LIVE_SERVER_ERROR_HAS_EXCEEDED_THE_LIMIT_IN_CONNECTION_OR_BATTLE:
                error_message =
                        context.getString(R.string.common_server_error_has_exceeded_the_limit_in_connection_or_battle);
                break;
            case LIVE_SERVER_ERROR_CREATING_CONNECTIONS_TOO_FREQUENT:
                error_message = context.getString(R.string.common_server_error_creating_connections_too_frequent);
                break;
            case LIVE_SERVER_ERROR_BATTLE_DOES_NOT_EXIST_OR_HAS_ENDED:
                error_message = context.getString(R.string.common_server_error_battle_does_not_exist_or_has_ended);
                break;
            case LIVE_SERVER_ERROR_NO_ROOMS_IN_THE_BATTLE_IS_VALID:
                error_message = context.getString(R.string.common_server_error_no_rooms_in_the_battle_is_valid);
                break;
            case LIVE_SERVER_ERROR_CREATING_BATTLES_TOO_FREQUENTLY:
                error_message = context.getString(R.string.common_server_error_creating_battles_too_frequently);
                break;
            case LIVE_SERVER_ERROR_THE_ROOM_IS_NOT_IN_THE_BATTLE:
                error_message = context.getString(R.string.common_server_error_the_room_is_not_in_the_battle);
                break;
            case LIVE_SERVER_ERROR_IN_OTHER_BATTLE:
                error_message = context.getString(R.string.common_server_error_in_other_battle);
                break;
            case LIVE_SERVER_ERROR_THERE_IS_A_PENDING_BATTLE_REQUEST:
                error_message = context.getString(R.string.common_server_error_there_is_a_pending_battle_request);
                break;
            case LIVE_SERVER_ERROR_IS_NOT_ALLOWED_TO_CANCEL_BATTLE_FOR_ROOM_IN_BATTLE:
                error_message =
                        context.getString(R.string.common_server_error_is_not_allowed_to_cancel_battle_for_room_in_battle);
                break;
            case LIVE_SERVER_ERROR_NOT_STARTED_YET:
                error_message = context.getString(R.string.common_server_error_not_started_yet);
                break;
            case LIVE_SERVER_ERROR_BATTLE_SESSION_HAS_ENDED:
                error_message = context.getString(R.string.common_server_error_battle_session_has_ended);
                break;
            case LIVE_SERVER_ERROR_METADATA_NUMBER_OF_KEYS_EXCEEDS_THE_LIMIT:
                error_message =
                        context.getString(R.string.common_server_error_metadata_number_of_keys_exceeds_the_limit);
                break;
            case LIVE_SERVER_ERROR_METADATA_SIZE_OF_VALUE_EXCEEDS_THE_LIMIT:
                error_message =
                        context.getString(R.string.common_server_error_metadata_size_of_value_exceeds_the_limit);
                break;
            case LIVE_SERVER_ERROR_METADATA_TOTAL_SIZE_EXCEEDS_THE_LIMIT:
                error_message = context.getString(R.string.common_server_error_metadata_total_size_exceeds_the_limit);
                break;
            case LIVE_SERVER_ERROR_METADATA_NO_VALID_KEYS:
                error_message = context.getString(R.string.common_server_error_metadata_no_valid_keys);
                break;
            case LIVE_SERVER_ERROR_METADATA_THE_SIZE_OF_KEY_EXCEEDS_THE_MAXIMUM_BYTE_LIMIT:
                error_message =
                        context.getString(R.string.common_server_error_metadata_the_size_of_key_exceeds_the_maximum_byte_limit);
                break;
            default:
                error_message = context.getString(R.string.common_client_error_failed) + error.getValue();
                break;
        }
        return error_message;
    }
}
