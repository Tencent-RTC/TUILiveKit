package com.trtc.uikit.livekit.component.giftaccess.store;

import com.trtc.uikit.livekit.component.giftaccess.service.GiftCacheService;

public class GiftStore {

    private static GiftStore sInstance;

    public final GiftCacheService mGiftCacheService = new GiftCacheService();

    public static GiftStore getInstance() {
        if (sInstance == null) {
            synchronized (GiftStore.class) {
                if (sInstance == null) {
                    sInstance = new GiftStore();
                }
            }
        }
        return sInstance;
    }
}
