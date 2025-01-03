package com.trtc.uikit.component.gift.store;

import com.trtc.uikit.component.common.StateCache;
import com.trtc.uikit.component.gift.service.GiftIMService;
import com.trtc.uikit.component.gift.service.LikeIMService;

import java.util.HashMap;
import java.util.Map;

public class GiftStore {

    private static final String STATE_KEY_GIFT = "key_state_gift";
    private static final String STATE_KEY_LIKE = "key_state_like";

    private static GiftStore sInstance;

    public final GiftIMService mGiftIMService = new GiftIMService();
    public final LikeIMService mLikeIMService = new LikeIMService();

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

    public GiftState getGiftState(String roomId) {
        StateCache stateCache = StateCache.getInstance();
        Map<String, Object> map = stateCache.get(roomId);
        if (map == null) {
            map = new HashMap<>();
            stateCache.set(roomId, map);
        }
        GiftState state = (GiftState) map.get(STATE_KEY_GIFT);
        if (state == null) {
            state = new GiftState();
            map.put(STATE_KEY_GIFT, state);
        }
        return state;
    }

    public LikeState getLikeState(String roomId) {
        StateCache stateCache = StateCache.getInstance();
        Map<String, Object> map = stateCache.get(roomId);
        if (map == null) {
            map = new HashMap<>();
            stateCache.set(roomId, map);
        }
        LikeState state = (LikeState) map.get(STATE_KEY_LIKE);
        if (state == null) {
            state = new LikeState();
            map.put(STATE_KEY_LIKE, state);
        }
        return state;
    }

    public boolean hasCachedRoomId(String roomId) {
        StateCache stateCache = StateCache.getInstance();
        return stateCache.get(roomId) != null;
    }
}
