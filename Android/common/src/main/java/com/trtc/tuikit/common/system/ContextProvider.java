package com.trtc.tuikit.common.system;

import android.content.Context;

import com.tencent.qcloud.tuicore.TUILogin;

public class ContextProvider {

    private static Context mApplicationContext;

    public static void setApplicationContext(Context context) {
        mApplicationContext = context.getApplicationContext();
    }

    public static Context getApplicationContext() {
        return mApplicationContext != null ? mApplicationContext : TUILogin.getAppContext();
    }
}