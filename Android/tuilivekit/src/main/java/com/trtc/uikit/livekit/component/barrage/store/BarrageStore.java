package com.trtc.uikit.livekit.component.barrage.store;

import androidx.core.util.Pair;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.component.barrage.store.model.Barrage;
import com.trtc.uikit.livekit.component.barrage.store.model.DefaultEmojiResource;
import com.trtc.uikit.livekit.component.barrage.service.IEmojiResource;

public class BarrageStore {
    private static BarrageStore sInstance;

    public LiveData<Pair<String, Barrage>> mSendBarrage = new LiveData<>();

    public final IEmojiResource mEmojiResource;

    public static BarrageStore sharedInstance() {
        if (sInstance == null) {
            synchronized (BarrageStore.class) {
                if (sInstance == null) {
                    sInstance = new BarrageStore();
                }
            }
        }
        return sInstance;
    }

    private BarrageStore() {
        mEmojiResource = new DefaultEmojiResource();
    }

}
