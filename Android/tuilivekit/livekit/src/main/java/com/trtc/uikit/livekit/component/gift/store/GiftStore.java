package com.trtc.uikit.livekit.component.gift.store;

import com.trtc.uikit.livekit.component.gift.service.GiftCacheService;
import com.trtc.uikit.livekit.component.gift.service.giftcloudserver.GiftCloudServer;

public class GiftStore {

    private static GiftStore sInstance;

    public final GiftCloudServer  mGiftCloudServer  = new GiftCloudServer();
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
