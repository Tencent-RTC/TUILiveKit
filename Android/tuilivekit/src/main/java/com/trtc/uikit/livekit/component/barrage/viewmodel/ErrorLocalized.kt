package com.trtc.uikit.livekit.component.barrage.viewmodel

import android.util.Log
import com.tencent.imsdk.BaseConstants
import com.trtc.tuikit.common.system.ContextProvider
import com.trtc.tuikit.common.util.ToastUtil
import com.trtc.uikit.livekit.R

object ErrorLocalized {
    private const val TAG = "BarrageErrorLocalized"

    fun onError(error: Int) {
        if (error == BaseConstants.ERR_SUCC) return

        val errorMessage = convertToErrorMessage(error)
        Log.e(TAG, "error:$error $errorMessage")
        ToastUtil.toastShortMessage(errorMessage)
    }

    private fun convertToErrorMessage(error: Int): String {
        val context = ContextProvider.getApplicationContext() ?: return error.toString()

        return when (error) {
            BaseConstants.ERR_SDK_COMM_API_CALL_FREQUENCY_LIMIT ->
                context.getString(R.string.live_barrage_error_freq_limit)

            BaseConstants.ERR_SVR_GROUP_SHUTUP_DENY ->
                context.getString(R.string.live_barrage_error_disable_message_by_admin)

            BaseConstants.ERR_SDK_BLOCKED_BY_SENSITIVE_WORD ->
                context.getString(R.string.live_barrage_error_sensitive_word)

            BaseConstants.ERR_SDK_NET_PKG_SIZE_LIMIT ->
                context.getString(R.string.live_barrage_error_content_is_long)

            BaseConstants.ERR_SDK_NET_DISCONNECT,
            BaseConstants.ERR_SDK_NET_WAIT_ACK_TIMEOUT,
            BaseConstants.ERR_SDK_NET_ALLREADY_CONN,
            BaseConstants.ERR_SDK_NET_CONN_TIMEOUT,
            BaseConstants.ERR_SDK_NET_CONN_REFUSE,
            BaseConstants.ERR_SDK_NET_NET_UNREACH,
            BaseConstants.ERR_SDK_NET_WAIT_INQUEUE_TIMEOUT,
            BaseConstants.ERR_SDK_NET_WAIT_SEND_TIMEOUT,
            BaseConstants.ERR_SDK_NET_WAIT_SEND_REMAINING_TIMEOUT,
            BaseConstants.ERR_SDK_NET_WAIT_SEND_TIMEOUT_NO_NETWORK,
            BaseConstants.ERR_SDK_NET_WAIT_ACK_TIMEOUT_NO_NETWORK,
            BaseConstants.ERR_SDK_NET_SEND_REMAINING_TIMEOUT_NO_NETWORK ->
                context.getString(R.string.live_barrage_error_network)

            else -> context.getString(R.string.live_barrage_error_failed) + error
        }
    }
}
