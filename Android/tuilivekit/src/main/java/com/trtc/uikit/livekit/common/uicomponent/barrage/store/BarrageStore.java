package com.trtc.uikit.livekit.common.uicomponent.barrage.store;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.TUIBarrage;
import com.trtc.uikit.livekit.common.uicomponent.barrage.model.DefaultEmojiResource;
import com.trtc.uikit.livekit.common.uicomponent.barrage.service.IEmojiResource;

public class BarrageStore {
    private static BarrageStore sInstance;

    public LiveData<TUIBarrage> mSendBarrage = new LiveData<>();

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
