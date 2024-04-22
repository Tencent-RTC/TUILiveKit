package com.trtc.uikit.livekit.common.core.store;

import com.trtc.uikit.livekit.common.core.LiveController;

public class LiveStore {
    private static volatile LiveStore      sInstance;
    private final           LiveController mLiveController;

    private LiveStore() {
        mLiveController = new LiveController();
    }

    public static LiveStore sharedInstance() {
        if (sInstance == null) {
            synchronized (LiveStore.class) {
                if (sInstance == null) {
                    sInstance = new LiveStore();
                }
            }
        }
        return sInstance;
    }

    public static void destroyInstance() {
        synchronized (LiveStore.class) {
            if (sInstance != null) {
                sInstance.destroy();
                sInstance = null;
            }
        }
    }

    private void destroy() {
        synchronized (LiveStore.class) {
            mLiveController.destroy();
        }
    }

    public LiveController getLiveController() {
        return mLiveController;
    }
}
