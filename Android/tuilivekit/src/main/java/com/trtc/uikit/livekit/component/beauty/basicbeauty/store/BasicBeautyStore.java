package com.trtc.uikit.livekit.component.beauty.basicbeauty.store;

import com.trtc.uikit.livekit.component.beauty.basicbeauty.service.BasicBeautyService;

public class BasicBeautyStore {
    private static BasicBeautyStore   sInstance;
    public final   BasicBeautyState   beautyState;
    public final   BasicBeautyService basicBeautyService;

    public static BasicBeautyStore getInstance() {
        if (sInstance == null) {
            synchronized (BasicBeautyStore.class) {
                if (sInstance == null) {
                    sInstance = new BasicBeautyStore();
                }
            }
        }
        return sInstance;
    }

    private BasicBeautyStore() {
        basicBeautyService = new BasicBeautyService();
        beautyState = basicBeautyService.getState();
    }

    public void init(String roomId) {
        beautyState.roomId = roomId;
    }

    public void unInit() {
        basicBeautyService.resetSettings();
    }
}
