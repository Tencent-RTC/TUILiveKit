package com.trtc.uikit.livekit.common.utils;

import android.content.Context;

import androidx.annotation.StringRes;

import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.util.ToastUtil;

public class ToastUtils {
    public static void toast(@StringRes int resourceId) {
        Context context = TUIConfig.getAppContext();
        if (context == null) {
            return;
        }
        if (resourceId == 0) {
            return;
        }
        ToastUtil.toastShortMessage(context.getString(resourceId));
    }
}
