package com.trtc.uikit.livekit.component.gift.store;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager.GiftInfo;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.gift.store.model.TUIGiftData;
import com.trtc.uikit.livekit.component.gift.store.model.TUILikeData;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TUIGiftStore {
    private static final LiveKitLogger LOGGER   = LiveKitLogger.getComponentLogger("TUIGiftStore");
    private static final TUIGiftStore  INSTANCE = new TUIGiftStore();

    public final MutableLiveData<String>                      mSystemLanguage    = new MutableLiveData<>("");
    public final MutableLiveData<Map<String, List<GiftInfo>>> mGiftListMap       = new MutableLiveData<>(new HashMap<>());
    public final MutableLiveData<Map<String, TUIGiftData>>    mGiftDataMap       = new MutableLiveData<>(new HashMap<>());
    public final MutableLiveData<Map<String, TUILikeData>>    mLikeDataMap       = new MutableLiveData<>(new HashMap<>());
    public final Map<String, Long>                            mLocalLikeCountMap = new HashMap<>();

    public static TUIGiftStore sharedInstance() {
        return INSTANCE;
    }

    public void unInit(String roomId) {
        LOGGER.info("unInit, roomId:" + roomId);
        mGiftListMap.getValue().remove(roomId);
        mGiftDataMap.getValue().remove(roomId);
        mLikeDataMap.getValue().remove(roomId);
        mLocalLikeCountMap.remove(roomId);
    }
}
