package com.trtc.uikit.livekit.common.uicomponent.gift.store;

import com.trtc.tuikit.common.livedata.LiveData;

public class GiftStore {

    private static GiftStore sInstance;

    public final LiveData<GiftSendData> mGiftSendData = new LiveData<>();

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
