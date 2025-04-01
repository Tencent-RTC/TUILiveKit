package com.trtc.uikit.livekit.component.gift.store;

import com.trtc.uikit.livekit.component.gift.service.GiftIMService;
import com.trtc.uikit.livekit.component.gift.service.LikeIMService;

import java.util.HashMap;
import java.util.Map;

public class GiftStore {
    private static GiftStore              sInstance;
    public final   GiftIMService          mGiftIMService = new GiftIMService();
    public final   LikeIMService          mLikeIMService = new LikeIMService();
    public final   Map<String, GiftState> mGiftStateMap  = new HashMap<>();
    public final   Map<String, LikeState> mLikeStateMap  = new HashMap<>();

    public static GiftStore sharedInstance() {
        if (sInstance == null) {
            synchronized (GiftStore.class) {
                if (sInstance == null) {
                    sInstance = new GiftStore();
                }
            }
        }
        return sInstance;
    }

    private GiftStore() {}

    public void init(String roomId) {
        getGiftState(roomId);
        getLikeState(roomId);
    }

    public void unInit(String roomId) {
        mGiftStateMap.remove(roomId);
        mLikeStateMap.remove(roomId);
    }

    public GiftState getGiftState(String roomId) {
        GiftState state = mGiftStateMap.get(roomId);
        if (state == null) {
            state = new GiftState();
            state.mRoomId = roomId;
            mGiftStateMap.put(roomId, state);
        }
        return state;
    }

    public LikeState getLikeState(String roomId) {
        LikeState state = mLikeStateMap.get(roomId);
        if (state == null) {
            state = new LikeState();
            state.mRoomId = roomId;
            mLikeStateMap.put(roomId, state);
        }
        return state;
    }

    public boolean hasCachedRoomId(String roomId) {
        return mGiftStateMap.containsKey(roomId);
    }
}
