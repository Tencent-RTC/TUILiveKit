package com.trtc.uikit.component.gift.store;

import androidx.core.util.Pair;

import com.trtc.tuikit.common.livedata.LiveData;

public class GiftStore {

    private static GiftStore sInstance;

    public final LiveData<Pair<String, GiftSendData>> mGiftSendData     = new LiveData<>();

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
