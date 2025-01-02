package com.trtc.uikit.component.barrage.store;

import com.trtc.uikit.component.barrage.service.BarrageIMService;
import com.trtc.uikit.component.barrage.service.IEmojiResource;
import com.trtc.uikit.component.barrage.store.model.DefaultEmojiResource;
import com.trtc.uikit.component.common.StateCache;

import java.util.HashMap;
import java.util.Map;

public class BarrageStore {
    private static BarrageStore sInstance;

    private static final String STATE_KEY = "key_state_barrage";

    public final IEmojiResource   mEmojiResource;
    public final BarrageIMService mBarrageIMService;

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

    public BarrageState getBarrageState(String roomId) {
        StateCache stateCache = StateCache.getInstance();
        Map<String, Object> map = stateCache.get(roomId);
        if (map == null) {
            map = new HashMap<>();
            stateCache.set(roomId, map);
        }
        BarrageState state = (BarrageState) map.get(STATE_KEY);
        if (state == null) {
            state = new BarrageState();
            map.put(STATE_KEY, state);
        }
        return state;
    }

    public boolean hasCachedRoomId(String roomId) {
        StateCache stateCache = StateCache.getInstance();
        return stateCache.get(roomId) != null;
    }
}
