package com.tencent.cloud.uikit.livekit.utils

import android.content.Context
import com.trtc.tuikit.common.util.ToastUtil

/**
 * This is a packaging class that can be replaced by internal implementations.
 * Prepare for future replacement implementation.
 */
object ToastUtils {
    fun showToast(context: Context, text: String) {
        ToastUtil.toastShortMessage(text)
    }
}