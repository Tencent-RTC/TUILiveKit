package com.tencent.effect.beautykit.utils;

import android.util.Log;


public class LogUtils {

    private static int logLevel = Log.WARN;

    public static void setLogLevel(int logLevel) {
        LogUtils.logLevel = logLevel;
    }

    public static void v(String tag, String msg) {
        if (logLevel <= Log.VERBOSE) {
            Log.v(tag, msg);
        }
    }

    public static void d(String tag, String msg) {
        if (logLevel <= Log.DEBUG) {
            Log.d(tag, msg);
        }
    }

    public static void i(String tag, String msg) {
        if (logLevel <= Log.INFO) {
            Log.i(tag, msg);
        }
    }

    public static void w(String tag, String msg) {
        if (logLevel <= Log.WARN) {
            Log.w(tag, msg);
        }
    }

    public static void e(String tag, String msg) {
        if (logLevel <= Log.ERROR) {
            Log.e(tag, msg);
        }
    }

}
