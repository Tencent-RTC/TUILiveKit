package com.trtc.tuikit.common.livedata;


import android.text.TextUtils;

import java.util.HashMap;
import java.util.Map;

public class LiveMapData<K, V> {
    private volatile Map<K, V>                   mData;
    private          Map<K, Observer<? super V>> mObserverMap = new HashMap<>();

    public LiveMapData(Map<K, V> map) {
        mData = map;
    }

    public void observe(K key, Observer<? super V> observer) {
        if (isEmpty(key)) {
            return;
        }
        if (observer == null) {
            return;
        }
        mObserverMap.put(key, observer);
    }

    public void removeObserver(K key) {
        if (isEmpty(key)) {
            return;
        }
        mObserverMap.remove(key);
    }

    public void removeAll() {
        mObserverMap.clear();
    }

    public void put(K key, V value) {
        if (isEmpty(key)) {
            return;
        }
        if (mData != null) {
            if (mData instanceof Map) {
                ((Map) mData).put(key, value);
                dispatchingValue(key, value);
            }
        }
    }

    public void remove(K key) {
        if (mData != null) {
            if (mData instanceof Map) {
                ((Map) mData).remove(key);

            }
        }
    }

    public void clear() {
        if (mData != null) {
            mData.clear();
            for (Map.Entry<K, V> entry : mData.entrySet()) {
                dispatchingValue(entry.getKey(), null);
            }
        }
    }

    public void putAll(Map<K, V> map) {
        if (mData != null) {
            mData.putAll(map);
            for (Map.Entry<K, V> entry : map.entrySet()) {
                dispatchingValue(entry.getKey(), entry.getValue());
            }
        }
    }

    public Map<K, V> get() {
        return mData;
    }

    private boolean isEmpty(K key) {
        if (key == null) {
            return true;
        }
        if (key instanceof String) {
            if (TextUtils.isEmpty((String) key)) {
                return true;
            }
        }
        return false;
    }

    private void dispatchingValue(K key, V value) {
        Observer<? super V> observer = mObserverMap.get(key);
        if (observer != null) {
            observer.onChanged(value);
        }
    }
}
