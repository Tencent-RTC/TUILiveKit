package com.trtc.uikit.component.common;

import java.util.HashMap;
import java.util.Map;

public final class StateCache {

    private static final StateCache INSTANCE = new StateCache();

    private final Map<String, Map<String, Object>> mCacheMap = new HashMap<>();

    public static StateCache getInstance() {
        return INSTANCE;
    }

    public void set(String key, Map<String, Object> object) {
        mCacheMap.put(key, object);
    }

    public Map<String, Object> get(String key) {
        return mCacheMap.get(key);
    }

    public void remove(String key) {
        Map<String, Object> cache = mCacheMap.remove(key);
        if (cache != null) {
            cache.clear();
        }
    }

    public void clear() {
        for (Map<String, Object> map : mCacheMap.values()) {
            if (map != null) {
                map.clear();
            }
        }
        mCacheMap.clear();
    }
}
