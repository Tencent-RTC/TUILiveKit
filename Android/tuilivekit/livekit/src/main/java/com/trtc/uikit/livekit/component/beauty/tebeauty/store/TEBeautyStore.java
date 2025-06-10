package com.trtc.uikit.livekit.component.beauty.tebeauty.store;

import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager;

public class TEBeautyStore {

    private static TEBeautyStore sInstance;

    public static TEBeautyStore getInstance() {
        if (sInstance == null) {
            synchronized (TEBeautyStore.class) {
                if (sInstance == null) {
                    sInstance = new TEBeautyStore();
                }
            }
        }
        return sInstance;
    }

    private TEBeautyStore() {

    }

    public void unInit() {
        TEBeautyManager.getInstance().clear();
    }
}
