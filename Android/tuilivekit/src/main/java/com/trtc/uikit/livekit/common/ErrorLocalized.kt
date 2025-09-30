package com.trtc.uikit.livekit.common

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.qcloud.tuicore.TUIConfig
import com.tencent.qcloud.tuicore.util.ToastUtil
import com.trtc.uikit.livekit.R

class ErrorLocalized {
    companion object {
        const val LIVE_CLIENT_ERROR_SUCCESS = 0
        const val LIVE_CLIENT_ERROR_FREQ_LIMIT = -2
        const val LIVE_CLIENT_ERROR_REPEAT_OPERATION = -3
        const val LIVE_CLIENT_ERROR_ROOM_MISMATCH = -4
        const val LIVE_CLIENT_ERROR_SDK_APP_ID_NOT_FOUND = -1000
        const val LIVE_CLIENT_ERROR_INVALID_PARAMETER = -1001
        const val LIVE_CLIENT_ERROR_SDK_NOT_INITIALIZED = -1002
        const val LIVE_CLIENT_ERROR_PERMISSION_DENIED = -1003
        const val LIVE_CLIENT_ERROR_REQUIRE_PAYMENT = -1004
        const val LIVE_CLIENT_ERROR_CAMERA_START_FAIL = -1100
        const val LIVE_CLIENT_ERROR_CAMERA_NOT_AUTHORIZED = -1101
        const val LIVE_CLIENT_ERROR_CAMERA_OCCUPIED = -1102
        const val LIVE_CLIENT_ERROR_CAMERA_DEVICE_EMPTY = -1103
        const val LIVE_CLIENT_ERROR_MICROPHONE_START_FAIL = -1104
        const val LIVE_CLIENT_ERROR_MICROPHONE_NOT_AUTHORIZED = -1105
        const val LIVE_CLIENT_ERROR_MICROPHONE_OCCUPIED = -1106
        const val LIVE_CLIENT_ERROR_MICROPHONE_DEVICE_EMPTY = -1107
        const val LIVE_CLIENT_ERROR_GET_SCREEN_SHARING_TARGET_FAILED = -1108
        const val LIVE_CLIENT_ERROR_START_SCREEN_SHARING_FAILED = -1109
        const val LIVE_CLIENT_ERROR_OPERATION_INVALID_BEFORE_ENTER_ROOM = -2101
        const val LIVE_CLIENT_ERROR_EXIT_NOT_SUPPORTED_FOR_ROOM_OWNER = -2102
        const val LIVE_CLIENT_ERROR_OPERATION_NOT_SUPPORTED_IN_CURRENT_ROOM_TYPE = -2103
        const val LIVE_CLIENT_ERROR_ROOM_ID_INVALID = -2105
        const val LIVE_CLIENT_ERROR_ROOM_NAME_INVALID = -2107
        const val LIVE_CLIENT_ERROR_ALREADY_IN_OTHER_ROOM = -2108
        const val LIVE_CLIENT_ERROR_USER_NOT_EXIST = -2200
        const val LIVE_CLIENT_ERROR_USER_NEED_OWNER_PERMISSION = -2300
        const val LIVE_CLIENT_ERROR_USER_NEED_ADMIN_PERMISSION = -2301
        const val LIVE_CLIENT_ERROR_REQUEST_NO_PERMISSION = -2310
        const val LIVE_CLIENT_ERROR_REQUEST_ID_INVALID = -2311
        const val LIVE_CLIENT_ERROR_REQUEST_ID_REPEAT = -2312
        const val LIVE_CLIENT_ERROR_MAX_SEAT_COUNT_LIMIT = -2340
        const val LIVE_CLIENT_ERROR_SEAT_INDEX_NOT_EXIST = -2344
        const val LIVE_CLIENT_ERROR_OPEN_MICROPHONE_NEED_SEAT_UNLOCK = -2360
        const val LIVE_CLIENT_ERROR_OPEN_MICROPHONE_NEED_PERMISSION_FROM_ADMIN = -2361
        const val LIVE_CLIENT_ERROR_OPEN_CAMERA_NEED_SEAT_UNLOCK = -2370
        const val LIVE_CLIENT_ERROR_OPEN_CAMERA_NEED_PERMISSION_FROM_ADMIN = -2371
        const val LIVE_CLIENT_ERROR_OPEN_SCREEN_SHARE_NEED_SEAT_UNLOCK = -2372
        const val LIVE_CLIENT_ERROR_OPEN_SCREEN_SHARE_NEED_PERMISSION_FROM_ADMIN = -2373
        const val LIVE_CLIENT_ERROR_SEND_MESSAGE_DISABLED_FOR_ALL = -2380
        const val LIVE_CLIENT_ERROR_SEND_MESSAGE_DISABLED_FOR_CURRENT = -2381
        const val LIVE_CLIENT_ERROR_ROOM_NOT_SUPPORT_PRELOADING = -4001
        const val LIVE_CLIENT_ERROR_CALL_IN_PROGRESS = -6001

        const val LIVE_SERVER_ERROR_SYSTEM_INTERNAL_ERROR = 100001
        const val LIVE_SERVER_ERROR_PARAM_ILLEGAL = 100002
        const val LIVE_SERVER_ERROR_ROOM_ID_EXISTS = 100003
        const val LIVE_SERVER_ERROR_ROOM_DOES_NOT_EXIST = 100004
        const val LIVE_SERVER_ERROR_NOT_A_ROOM_MEMBER = 100005
        const val LIVE_SERVER_ERROR_INSUFFICIENT_OPERATION_PERMISSIONS = 100006
        const val LIVE_SERVER_ERROR_NO_PAYMENT_INFORMATION = 100007
        const val LIVE_SERVER_ERROR_ROOM_IS_FULL = 100008
        const val LIVE_SERVER_ERROR_TAG_QUANTITY_EXCEEDS_UPPER_LIMIT = 100009
        const val LIVE_SERVER_ERROR_ROOM_ID_HAS_BEEN_USED = 100010
        const val LIVE_SERVER_ERROR_ROOM_ID_HAS_BEEN_OCCUPIED_BY_CHAT = 100011
        const val LIVE_SERVER_ERROR_CREATING_ROOMS_EXCEEDS_THE_FREQUENCY_LIMIT = 100012
        const val LIVE_SERVER_ERROR_EXCEEDS_THE_UPPER_LIMIT = 100013
        const val LIVE_SERVER_ERROR_INVALID_ROOM_TYPE = 100015
        const val LIVE_SERVER_ERROR_THIS_MEMBER_HAS_BEEN_BANNED = 100016
        const val LIVE_SERVER_ERROR_THIS_MEMBER_HAS_BEEN_MUTED = 100017
        const val LIVE_SERVER_ERROR_REQUIRES_PASSWORD = 100018
        const val LIVE_SERVER_ERROR_ROOM_ENTRY_PASSWORD_ERROR = 100019
        const val LIVE_SERVER_ERROR_ROOM_ADMIN_QUANTITY_EXCEEDS_THE_UPPER_LIMIT = 100020
        const val LIVE_SERVER_ERROR_SIGNAL_REQUEST_CONFLICT = 100102
        const val LIVE_SERVER_ERROR_MIC_SEAT_IS_LOCKED = 100200
        const val LIVE_SERVER_ERROR_SEAT_IS_ALREADY_OCCUPIED = 100201
        const val LIVE_SERVER_ERROR_ALREADY_ON_THE_MIC_QUEUE = 100202
        const val LIVE_SERVER_ERROR_ALREADY_ON_THE_MIC = 100203
        const val LIVE_SERVER_ERROR_NOT_ON_THE_MIC_QUEUE = 100204
        const val LIVE_SERVER_ERROR_THE_SEATS_ARE_ALL_TAKEN = 100205
        const val LIVE_SERVER_ERROR_NOT_ON_THE_MIC_SEAT = 100206
        const val LIVE_SERVER_ERROR_USER_IS_ALREADY_ON_THE_MIC_SEAT = 100210
        const val LIVE_SERVER_ERROR_ROOM_DOES_NOT_SUPPORT_MIC_ABILITY = 100211
        const val LIVE_SERVER_ERROR_THE_SEAT_LIST_IS_EMPTY = 100251
        const val LIVE_SERVER_ERROR_CONNECTION_DOES_NOT_EXIST = 100400
        const val LIVE_SERVER_ERROR_ROOM_IS_IN_CONNECTION = 100401
        const val LIVE_SERVER_ERROR_THERE_IS_A_PENDING_CONNECTION_REQUEST = 100402
        const val LIVE_SERVER_ERROR_IS_CONNECTING_WITH_OTHER_ROOMS = 100403
        const val LIVE_SERVER_ERROR_HAS_EXCEEDED_THE_LIMIT_IN_CONNECTION_OR_BATTLE = 100404
        const val LIVE_SERVER_ERROR_CREATING_CONNECTIONS_TOO_FREQUENT = 100405
        const val LIVE_SERVER_ERROR_BATTLE_DOES_NOT_EXIST_OR_HAS_ENDED = 100411
        const val LIVE_SERVER_ERROR_NO_ROOMS_IN_THE_BATTLE_IS_VALID = 100412
        const val LIVE_SERVER_ERROR_CREATING_BATTLES_TOO_FREQUENTLY = 100413
        const val LIVE_SERVER_ERROR_THE_ROOM_IS_NOT_IN_THE_BATTLE = 100414
        const val LIVE_SERVER_ERROR_IN_OTHER_BATTLE = 100415
        const val LIVE_SERVER_ERROR_THERE_IS_A_PENDING_BATTLE_REQUEST = 100416
        const val LIVE_SERVER_ERROR_IS_NOT_ALLOWED_TO_CANCEL_BATTLE_FOR_ROOM_IN_BATTLE = 100419
        const val LIVE_SERVER_ERROR_NOT_STARTED_YET = 100420
        const val LIVE_SERVER_ERROR_BATTLE_SESSION_HAS_ENDED = 100421
        const val LIVE_SERVER_ERROR_METADATA_NUMBER_OF_KEYS_EXCEEDS_THE_LIMIT = 100500
        const val LIVE_SERVER_ERROR_METADATA_SIZE_OF_VALUE_EXCEEDS_THE_LIMIT = 100501
        const val LIVE_SERVER_ERROR_METADATA_TOTAL_SIZE_EXCEEDS_THE_LIMIT = 100502
        const val LIVE_SERVER_ERROR_METADATA_NO_VALID_KEYS = 100503
        const val LIVE_SERVER_ERROR_METADATA_THE_SIZE_OF_KEY_EXCEEDS_THE_MAXIMUM_BYTE_LIMIT = 100504

        private val INTERCEPT_TOAST_ONLY_PRINT_LOG = setOf(LIVE_CLIENT_ERROR_ROOM_MISMATCH)
        private val LOGGER = LiveKitLogger.getCommonLogger("ErrorLocalized")

        @JvmStatic
        fun onError(error: TUICommonDefine.Error) {
            if (error == TUICommonDefine.Error.SUCCESS) return
            convertToErrorMessage(error).also { message ->
                LOGGER.info("[error:$error,value:${error.value},message:$message]")
                if (!INTERCEPT_TOAST_ONLY_PRINT_LOG.contains(error.value)) {
                    ToastUtil.toastShortMessage(message)
                }
            }
        }

        private fun convertToErrorMessage(error: TUICommonDefine.Error): String {
            val context = TUIConfig.getAppContext() ?: return ""
            return when (error.value) {
                LIVE_CLIENT_ERROR_SUCCESS ->
                    context.getString(R.string.common_client_error_success)

                LIVE_CLIENT_ERROR_FREQ_LIMIT ->
                    context.getString(R.string.common_client_error_freq_limit)

                LIVE_CLIENT_ERROR_REPEAT_OPERATION ->
                    context.getString(R.string.common_client_error_repeat_operation)

                LIVE_CLIENT_ERROR_SDK_APP_ID_NOT_FOUND ->
                    context.getString(R.string.common_client_error_sdk_app_id_not_found)

                LIVE_CLIENT_ERROR_INVALID_PARAMETER ->
                    context.getString(R.string.common_client_error_invalid_parameter)

                LIVE_CLIENT_ERROR_SDK_NOT_INITIALIZED ->
                    context.getString(R.string.common_client_error_sdk_not_initialized)

                LIVE_CLIENT_ERROR_PERMISSION_DENIED ->
                    context.getString(R.string.common_client_error_permission_denied)

                LIVE_CLIENT_ERROR_REQUIRE_PAYMENT ->
                    context.getString(R.string.common_client_error_require_payment)

                LIVE_CLIENT_ERROR_CAMERA_START_FAIL ->
                    context.getString(R.string.common_client_error_camera_start_fail)

                LIVE_CLIENT_ERROR_CAMERA_NOT_AUTHORIZED ->
                    context.getString(R.string.common_client_error_camera_not_authorized)

                LIVE_CLIENT_ERROR_CAMERA_OCCUPIED ->
                    context.getString(R.string.common_client_error_camera_occupied)

                LIVE_CLIENT_ERROR_CAMERA_DEVICE_EMPTY ->
                    context.getString(R.string.common_client_error_camera_device_empty)

                LIVE_CLIENT_ERROR_MICROPHONE_START_FAIL ->
                    context.getString(R.string.common_client_error_microphone_start_fail)

                LIVE_CLIENT_ERROR_MICROPHONE_NOT_AUTHORIZED ->
                    context.getString(R.string.common_client_error_microphone_not_authorized)

                LIVE_CLIENT_ERROR_MICROPHONE_OCCUPIED ->
                    context.getString(R.string.common_client_error_microphone_occupied)

                LIVE_CLIENT_ERROR_MICROPHONE_DEVICE_EMPTY ->
                    context.getString(R.string.common_client_error_microphone_device_empty)

                LIVE_CLIENT_ERROR_GET_SCREEN_SHARING_TARGET_FAILED ->
                    context.getString(R.string.common_client_error_get_screen_sharing_target_failed)

                LIVE_CLIENT_ERROR_START_SCREEN_SHARING_FAILED ->
                    context.getString(R.string.common_client_error_start_screen_sharing_failed)

                LIVE_CLIENT_ERROR_OPERATION_INVALID_BEFORE_ENTER_ROOM ->
                    context.getString(R.string.common_client_error_operation_invalid_before_enter_room)

                LIVE_CLIENT_ERROR_EXIT_NOT_SUPPORTED_FOR_ROOM_OWNER ->
                    context.getString(R.string.common_client_error_exit_not_supported_for_room_owner)

                LIVE_CLIENT_ERROR_OPERATION_NOT_SUPPORTED_IN_CURRENT_ROOM_TYPE ->
                    context.getString(R.string.common_client_error_operation_not_supported_in_current_room_type)

                LIVE_CLIENT_ERROR_ROOM_ID_INVALID ->
                    context.getString(R.string.common_client_error_room_id_invalid)

                LIVE_CLIENT_ERROR_ROOM_NAME_INVALID ->
                    context.getString(R.string.common_client_error_room_name_invalid)

                LIVE_CLIENT_ERROR_ALREADY_IN_OTHER_ROOM ->
                    context.getString(R.string.common_client_error_already_in_other_room)

                LIVE_CLIENT_ERROR_USER_NOT_EXIST ->
                    context.getString(R.string.common_client_error_user_not_exist)

                LIVE_CLIENT_ERROR_USER_NEED_OWNER_PERMISSION ->
                    context.getString(R.string.common_client_error_user_need_owner_permission)

                LIVE_CLIENT_ERROR_USER_NEED_ADMIN_PERMISSION ->
                    context.getString(R.string.common_client_error_user_need_admin_permission)

                LIVE_CLIENT_ERROR_REQUEST_NO_PERMISSION ->
                    context.getString(R.string.common_client_error_request_no_permission)

                LIVE_CLIENT_ERROR_REQUEST_ID_INVALID ->
                    context.getString(R.string.common_client_error_request_id_invalid)

                LIVE_CLIENT_ERROR_REQUEST_ID_REPEAT ->
                    context.getString(R.string.common_client_error_request_id_repeat)

                LIVE_CLIENT_ERROR_MAX_SEAT_COUNT_LIMIT ->
                    context.getString(R.string.common_client_error_max_seat_count_limit)

                LIVE_CLIENT_ERROR_SEAT_INDEX_NOT_EXIST ->
                    context.getString(R.string.common_client_error_seat_index_not_exist)

                LIVE_CLIENT_ERROR_OPEN_MICROPHONE_NEED_SEAT_UNLOCK ->
                    context.getString(R.string.common_client_error_open_microphone_need_seat_unlock)

                LIVE_CLIENT_ERROR_OPEN_MICROPHONE_NEED_PERMISSION_FROM_ADMIN ->
                    context.getString(R.string.common_client_error_open_microphone_need_permission_from_admin)

                LIVE_CLIENT_ERROR_OPEN_CAMERA_NEED_SEAT_UNLOCK ->
                    context.getString(R.string.common_client_error_open_camera_need_seat_unlock)

                LIVE_CLIENT_ERROR_OPEN_CAMERA_NEED_PERMISSION_FROM_ADMIN ->
                    context.getString(R.string.common_client_error_open_camera_need_permission_from_admin)

                LIVE_CLIENT_ERROR_OPEN_SCREEN_SHARE_NEED_SEAT_UNLOCK ->
                    context.getString(R.string.common_client_error_open_screen_share_need_seat_unlock)

                LIVE_CLIENT_ERROR_OPEN_SCREEN_SHARE_NEED_PERMISSION_FROM_ADMIN ->
                    context.getString(R.string.common_client_error_open_screen_share_need_permission_from_admin)

                LIVE_CLIENT_ERROR_SEND_MESSAGE_DISABLED_FOR_ALL ->
                    context.getString(R.string.common_client_error_send_message_disabled_for_all)

                LIVE_CLIENT_ERROR_SEND_MESSAGE_DISABLED_FOR_CURRENT ->
                    context.getString(R.string.common_client_error_send_message_disabled_for_current)

                LIVE_CLIENT_ERROR_ROOM_NOT_SUPPORT_PRELOADING ->
                    context.getString(R.string.common_client_error_room_not_support_preloading)

                LIVE_CLIENT_ERROR_CALL_IN_PROGRESS ->
                    context.getString(R.string.common_server_error_call_in_progress)

                LIVE_SERVER_ERROR_SYSTEM_INTERNAL_ERROR ->
                    context.getString(R.string.common_server_error_system_internal_error)

                LIVE_SERVER_ERROR_PARAM_ILLEGAL ->
                    context.getString(R.string.common_server_error_param_illegal)

                LIVE_SERVER_ERROR_ROOM_ID_EXISTS ->
                    context.getString(R.string.common_server_error_room_id_exists)

                LIVE_SERVER_ERROR_ROOM_DOES_NOT_EXIST ->
                    context.getString(R.string.common_server_error_room_does_not_exist)

                LIVE_SERVER_ERROR_NOT_A_ROOM_MEMBER ->
                    context.getString(R.string.common_server_error_not_a_room_member)

                LIVE_SERVER_ERROR_INSUFFICIENT_OPERATION_PERMISSIONS ->
                    context.getString(R.string.common_server_error_insufficient_operation_permissions)

                LIVE_SERVER_ERROR_NO_PAYMENT_INFORMATION ->
                    context.getString(R.string.common_server_error_no_payment_information)

                LIVE_SERVER_ERROR_ROOM_IS_FULL ->
                    context.getString(R.string.common_server_error_room_is_full)

                LIVE_SERVER_ERROR_TAG_QUANTITY_EXCEEDS_UPPER_LIMIT ->
                    context.getString(R.string.common_server_error_tag_quantity_exceeds_upper_limit)

                LIVE_SERVER_ERROR_ROOM_ID_HAS_BEEN_USED ->
                    context.getString(R.string.common_server_error_room_id_has_been_used)

                LIVE_SERVER_ERROR_ROOM_ID_HAS_BEEN_OCCUPIED_BY_CHAT ->
                    context.getString(R.string.common_server_error_room_id_has_been_occupied_by_chat)

                LIVE_SERVER_ERROR_CREATING_ROOMS_EXCEEDS_THE_FREQUENCY_LIMIT ->
                    context.getString(R.string.common_server_error_creating_rooms_exceeds_the_frequency_limit)

                LIVE_SERVER_ERROR_EXCEEDS_THE_UPPER_LIMIT ->
                    context.getString(R.string.common_server_error_exceeds_the_upper_limit)

                LIVE_SERVER_ERROR_INVALID_ROOM_TYPE ->
                    context.getString(R.string.common_server_error_invalid_room_type)

                LIVE_SERVER_ERROR_THIS_MEMBER_HAS_BEEN_BANNED ->
                    context.getString(R.string.common_server_error_this_member_has_been_banned)

                LIVE_SERVER_ERROR_THIS_MEMBER_HAS_BEEN_MUTED ->
                    context.getString(R.string.common_server_error_this_member_has_been_muted)

                LIVE_SERVER_ERROR_REQUIRES_PASSWORD ->
                    context.getString(R.string.common_server_error_requires_password)

                LIVE_SERVER_ERROR_ROOM_ENTRY_PASSWORD_ERROR ->
                    context.getString(R.string.common_server_error_room_entry_password_error)

                LIVE_SERVER_ERROR_ROOM_ADMIN_QUANTITY_EXCEEDS_THE_UPPER_LIMIT ->
                    context.getString(R.string.common_server_error_room_admin_quantity_exceeds_the_upper_limit)

                LIVE_SERVER_ERROR_SIGNAL_REQUEST_CONFLICT ->
                    context.getString(R.string.common_server_error_signal_request_conflict)

                LIVE_SERVER_ERROR_MIC_SEAT_IS_LOCKED ->
                    context.getString(R.string.common_server_error_mic_seat_is_locked)

                LIVE_SERVER_ERROR_SEAT_IS_ALREADY_OCCUPIED ->
                    context.getString(R.string.common_server_error_seat_is_already_occupied)

                LIVE_SERVER_ERROR_ALREADY_ON_THE_MIC_QUEUE ->
                    context.getString(R.string.common_server_error_already_on_the_mic_queue)

                LIVE_SERVER_ERROR_ALREADY_ON_THE_MIC ->
                    context.getString(R.string.common_server_error_already_on_the_mic)

                LIVE_SERVER_ERROR_NOT_ON_THE_MIC_QUEUE ->
                    context.getString(R.string.common_server_error_not_on_the_mic_queue)

                LIVE_SERVER_ERROR_THE_SEATS_ARE_ALL_TAKEN ->
                    context.getString(R.string.common_server_error_the_seats_are_all_taken)

                LIVE_SERVER_ERROR_NOT_ON_THE_MIC_SEAT ->
                    context.getString(R.string.common_server_error_not_on_the_mic_seat)

                LIVE_SERVER_ERROR_USER_IS_ALREADY_ON_THE_MIC_SEAT ->
                    context.getString(R.string.common_server_error_user_is_already_on_the_mic_seat)

                LIVE_SERVER_ERROR_ROOM_DOES_NOT_SUPPORT_MIC_ABILITY ->
                    context.getString(R.string.common_server_error_room_does_not_support_mic_ability)

                LIVE_SERVER_ERROR_THE_SEAT_LIST_IS_EMPTY ->
                    context.getString(R.string.common_server_error_the_seat_list_is_empty)

                LIVE_SERVER_ERROR_CONNECTION_DOES_NOT_EXIST ->
                    context.getString(R.string.common_server_error_connection_does_not_exist)

                LIVE_SERVER_ERROR_ROOM_IS_IN_CONNECTION ->
                    context.getString(R.string.common_server_error_room_is_in_connection)

                LIVE_SERVER_ERROR_THERE_IS_A_PENDING_CONNECTION_REQUEST ->
                    context.getString(R.string.common_server_error_there_is_a_pending_connection_request)

                LIVE_SERVER_ERROR_IS_CONNECTING_WITH_OTHER_ROOMS ->
                    context.getString(R.string.common_server_error_is_connecting_with_other_rooms)

                LIVE_SERVER_ERROR_HAS_EXCEEDED_THE_LIMIT_IN_CONNECTION_OR_BATTLE ->
                    context.getString(R.string.common_server_error_has_exceeded_the_limit_in_connection_or_battle)

                LIVE_SERVER_ERROR_CREATING_CONNECTIONS_TOO_FREQUENT ->
                    context.getString(R.string.common_server_error_creating_connections_too_frequent)

                LIVE_SERVER_ERROR_BATTLE_DOES_NOT_EXIST_OR_HAS_ENDED ->
                    context.getString(R.string.common_server_error_battle_does_not_exist_or_has_ended)

                LIVE_SERVER_ERROR_NO_ROOMS_IN_THE_BATTLE_IS_VALID ->
                    context.getString(R.string.common_server_error_no_rooms_in_the_battle_is_valid)

                LIVE_SERVER_ERROR_CREATING_BATTLES_TOO_FREQUENTLY ->
                    context.getString(R.string.common_server_error_creating_battles_too_frequently)

                LIVE_SERVER_ERROR_THE_ROOM_IS_NOT_IN_THE_BATTLE ->
                    context.getString(R.string.common_server_error_the_room_is_not_in_the_battle)

                LIVE_SERVER_ERROR_IN_OTHER_BATTLE ->
                    context.getString(R.string.common_server_error_in_other_battle)

                LIVE_SERVER_ERROR_THERE_IS_A_PENDING_BATTLE_REQUEST ->
                    context.getString(R.string.common_server_error_there_is_a_pending_battle_request)

                LIVE_SERVER_ERROR_IS_NOT_ALLOWED_TO_CANCEL_BATTLE_FOR_ROOM_IN_BATTLE ->
                    context.getString(R.string.common_server_error_is_not_allowed_to_cancel_battle_for_room_in_battle)

                LIVE_SERVER_ERROR_NOT_STARTED_YET ->
                    context.getString(R.string.common_server_error_not_started_yet)

                LIVE_SERVER_ERROR_BATTLE_SESSION_HAS_ENDED ->
                    context.getString(R.string.common_server_error_battle_session_has_ended)

                LIVE_SERVER_ERROR_METADATA_NUMBER_OF_KEYS_EXCEEDS_THE_LIMIT ->
                    context.getString(R.string.common_server_error_metadata_number_of_keys_exceeds_the_limit)

                LIVE_SERVER_ERROR_METADATA_SIZE_OF_VALUE_EXCEEDS_THE_LIMIT ->
                    context.getString(R.string.common_server_error_metadata_size_of_value_exceeds_the_limit)

                LIVE_SERVER_ERROR_METADATA_TOTAL_SIZE_EXCEEDS_THE_LIMIT ->
                    context.getString(R.string.common_server_error_metadata_total_size_exceeds_the_limit)

                LIVE_SERVER_ERROR_METADATA_NO_VALID_KEYS ->
                    context.getString(R.string.common_server_error_metadata_no_valid_keys)

                LIVE_SERVER_ERROR_METADATA_THE_SIZE_OF_KEY_EXCEEDS_THE_MAXIMUM_BYTE_LIMIT ->
                    context.getString(R.string.common_server_error_metadata_the_size_of_key_exceeds_the_maximum_byte_limit)

                else ->
                    context.getString(R.string.common_client_error_failed) + error.value
            }
        }
    }
}