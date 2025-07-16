package com.trtc.uikit.livekit.component.barrage.service;

import android.content.Context;
import android.util.Log;

import com.tencent.imsdk.BaseConstants;
import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.tuikit.common.util.ToastUtil;
import com.trtc.uikit.livekit.R;

public class ErrorLocalized {

    private static final String TAG = "BarrageErrorLocalized";

    public static void onError(int error) {
        if (error == BaseConstants.ERR_SUCC) {
            return;
        }
        String errorMessage = convertToErrorMessage(error);
        Log.e(TAG, "error:" + error + " " + errorMessage);
        ToastUtil.toastShortMessage(errorMessage);
    }

    private static String convertToErrorMessage(int error) {
        String message = "";
        Context context = ContextProvider.getApplicationContext();
        if (context == null) {
            return "" + error;
        }
        switch (error) {
            case BaseConstants.ERR_SDK_COMM_API_CALL_FREQUENCY_LIMIT:
                message = context.getString(R.string.live_barrage_error_freq_limit);
                break;
            case BaseConstants.ERR_SVR_GROUP_SHUTUP_DENY:
                message = context.getString(R.string.live_barrage_error_disable_message_by_admin);
                break;
            case BaseConstants.ERR_SDK_BLOCKED_BY_SENSITIVE_WORD:
                message = context.getString(R.string.live_barrage_error_sensitive_word);
                break;
            case BaseConstants.ERR_SDK_NET_PKG_SIZE_LIMIT:
                message = context.getString(R.string.live_barrage_error_content_is_long);
                break;
            case BaseConstants.ERR_SDK_NET_DISCONNECT:
            case BaseConstants.ERR_SDK_NET_WAIT_ACK_TIMEOUT:
            case BaseConstants.ERR_SDK_NET_ALLREADY_CONN:
            case BaseConstants.ERR_SDK_NET_CONN_TIMEOUT:
            case BaseConstants.ERR_SDK_NET_CONN_REFUSE:
            case BaseConstants.ERR_SDK_NET_NET_UNREACH:
            case BaseConstants.ERR_SDK_NET_WAIT_INQUEUE_TIMEOUT:
            case BaseConstants.ERR_SDK_NET_WAIT_SEND_TIMEOUT:
            case BaseConstants.ERR_SDK_NET_WAIT_SEND_REMAINING_TIMEOUT:
            case BaseConstants.ERR_SDK_NET_WAIT_SEND_TIMEOUT_NO_NETWORK:
            case BaseConstants.ERR_SDK_NET_WAIT_ACK_TIMEOUT_NO_NETWORK:
            case BaseConstants.ERR_SDK_NET_SEND_REMAINING_TIMEOUT_NO_NETWORK:
                message = context.getString(R.string.live_barrage_error_network);
                break;
            default:
                message = context.getString(R.string.live_barrage_error_failed) + error;
        }
        return message;
    }
}

