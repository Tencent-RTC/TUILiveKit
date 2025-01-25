package com.trtc.uikit.component.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class StateCache {

    private static final StateCache INSTANCE = new StateCache();

    private final Map<String, Map<String, Object>> mCacheMap        = new HashMap<>();
    private final Map<String, List<Runnable>>      mRemoveActionMap = new HashMap<>();

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
        callRemoveAction(key);
    }

    public void clear() {
        for (String key : mCacheMap.keySet()) {
            Map<String, Object> map = mCacheMap.get(key);
            if (map != null) {
                map.clear();
            }
            callRemoveAction(key);
        }
        mCacheMap.clear();
        mRemoveActionMap.clear();
    }

    public void subscribeToObjectRemoval(String key, Runnable removeAction) {
        List<Runnable> removeActions = mRemoveActionMap.get(key);
        if (removeActions == null) {
            removeActions = new ArrayList<>();
            mRemoveActionMap.put(key, removeActions);
        }
        removeActions.add(removeAction);
    }

    public void unsubscribeToObjectRemoval(String key) {
        List<Runnable> removeActions = mRemoveActionMap.remove(key);
        if (removeActions != null) {
            removeActions.clear();
        }
    }

    private void callRemoveAction(String key) {
        List<Runnable> removeActions = mRemoveActionMap.remove(key);
        if (removeActions != null) {
            for (Runnable action : removeActions) {
                action.run();
            }
            removeActions.clear();
        }
    }
}
