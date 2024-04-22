package com.trtc.uikit.livekit.common.utils;

import com.tencent.liteav.basic.log.TXCLog;

public class LiveKitLog {
    private static final String TAG = "LiveKitLog";

    public static void error(String message) {
        TXCLog.e(TAG, message);
    }

    public static void warn(String message) {
        TXCLog.w(TAG, message);
    }

    public static void info(String message) {
        TXCLog.i(TAG, message);
    }

    public static void debug(String message) {
        TXCLog.d(TAG, message);
    }
}
