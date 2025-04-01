package com.trtc.uikit.livekit.component.gift.access.store;

import com.trtc.uikit.livekit.component.gift.access.service.GiftCacheService;
import com.trtc.uikit.livekit.component.gift.access.service.giftcloudserver.GiftCloudServer;

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
