package com.trtc.uikit.livekit.common.uicomponent.gift.store;

import androidx.core.util.Pair;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.GiftCloudServer;
import com.trtc.uikit.livekit.common.uicomponent.gift.giftcloudserver.IGiftCloudServer;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftCacheService;

public class GiftStore {

    private static GiftStore sInstance;

    public final LiveData<Pair<String, GiftSendData>> mGiftSendData     = new LiveData<>();
    public final IGiftCloudServer                     mGiftCloudServer  = new GiftCloudServer();
    public final GiftCacheService                     mGiftCacheService = new GiftCacheService();

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
