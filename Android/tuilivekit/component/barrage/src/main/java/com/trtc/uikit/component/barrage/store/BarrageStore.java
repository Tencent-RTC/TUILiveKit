package com.trtc.uikit.component.barrage.store;

import com.trtc.uikit.component.barrage.service.BarrageIMService;
import com.trtc.uikit.component.barrage.service.IEmojiResource;
import com.trtc.uikit.component.barrage.store.model.DefaultEmojiResource;

import java.util.HashMap;
import java.util.Map;

public class BarrageStore {
    private static BarrageStore              sInstance;
    public final   IEmojiResource            mEmojiResource;
    public final   BarrageIMService          mBarrageIMService;
    public final   Map<String, BarrageState> mBarrageStateMap = new HashMap<>();

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
        mBarrageIMService = new BarrageIMService();
    }

    public void init(String roomId, String ownerId) {
        BarrageState state = mBarrageStateMap.get(roomId);
        if (state == null) {
            state = new BarrageState();
            state.mRoomId = roomId;
            mBarrageStateMap.put(roomId, state);
        }
        state.mOwnerId = ownerId;
    }

    public void unInit(String roomId) {
        mBarrageStateMap.remove(roomId);
    }

    public boolean hasCachedRoomId(String roomId) {
        return mBarrageStateMap.containsKey(roomId);
    }

    public BarrageState getBarrageState(String roomId) {
        BarrageState barrageState = mBarrageStateMap.get(roomId);
        if (barrageState == null) {
            barrageState = new BarrageState();
            barrageState.mRoomId = roomId;
            mBarrageStateMap.put(roomId, barrageState);
        }
        return barrageState;
    }
}
