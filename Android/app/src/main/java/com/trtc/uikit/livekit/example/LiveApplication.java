package com.trtc.uikit.livekit.example;

import android.app.Application;

import com.trtc.uikit.livekit.example.view.crash.TUILiveKitCrashHandler;

public class LiveApplication extends Application {
    @Override
    public void onCreate() {
        super.onCreate();

        TUILiveKitCrashHandler.getInstance().initialize(this);
    }
}
