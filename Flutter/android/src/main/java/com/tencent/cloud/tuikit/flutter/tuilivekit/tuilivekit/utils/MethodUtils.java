package com.tencent.cloud.tuikit.flutter.tuilivekit.tuilivekit.utils;

import android.util.Log;

import io.flutter.plugin.common.MethodCall;
import io.flutter.plugin.common.MethodChannel;

public class MethodUtils {
    private static final String TAG = "TUILiveKitFlutter";

    /**
     * General method, get parameter values, if no parameters are found, it will be interrupted directly
     */
    public static <T> T getMethodRequiredParams(MethodCall methodCall, String param, MethodChannel.Result result) {
        T parameter = methodCall.argument(param);
        if (parameter == null) {
            result.error("Missing parameter",
                    "Cannot find parameter `" + param + "` or `" + param + "` is null!",
                    -1001);
            Log.e(TAG, "|method=" + methodCall.method + "|arguments=null");
        }
        return parameter;
    }

    /**
     * General method, get parameter values, parameters can be null
     */
    public static <T> T getMethodParams(MethodCall methodCall, String param) {
        T parameter = methodCall.argument(param);
        return parameter;
    }
}
