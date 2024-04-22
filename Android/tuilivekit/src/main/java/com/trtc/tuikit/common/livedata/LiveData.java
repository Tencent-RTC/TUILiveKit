package com.trtc.tuikit.common.livedata;


import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class LiveData<T> {
    private static final Object DEFAULT_OBJECT = new Object();

    private volatile Object                    mData;
    private          List<Observer<? super T>> mObservers = new ArrayList<>();

    public LiveData(T value) {
        mData = value;
    }

    public LiveData() {
        mData = DEFAULT_OBJECT;
    }

    public void observe(Observer<? super T> observer) {
        if (observer == null || mObservers.contains(observer)) {
            return;
        }
        mObservers.add(observer);
    }

    public void removeObserver(final Observer<? super T> observer) {
        mObservers.remove(observer);
    }

    public void removeAll() {
        mObservers.clear();
    }

    public void set(T value) {
        set(value, true);
    }

    public void set(T value, boolean isForceNotify) {
        if (isForceNotify || !mData.equals(value)) {
            mData = value;
            dispatchingValue();
        }
    }

    public void add(Object item) {
        if (mData != null) {
            if (mData instanceof HashSet) {
                HashSet hashSet = (HashSet) mData;
                int oldSize = hashSet.size();
                hashSet.add(item);
                if (oldSize != hashSet.size()) {
                    dispatchingValue();
                }
            } else if (mData instanceof List) {
                ((List) mData).add(item);
                dispatchingValue();
            } else if (mData instanceof Set) {
                ((Set) mData).add(item);
                dispatchingValue();
            }
        }
    }

    public void addAll(Collection item) {
        if (mData != null) {
            if (mData instanceof List) {
                ((List) mData).addAll(item);
                dispatchingValue();
            } else if (mData instanceof Set) {
                ((Set) mData).addAll(item);
                dispatchingValue();
            }
        }
    }

    public void remove(Object item) {
        if (mData != null) {
            if (mData instanceof HashSet) {
                HashSet hashSet = (HashSet) mData;
                int oldSize = hashSet.size();
                hashSet.remove(item);
                if (oldSize != hashSet.size()) {
                    dispatchingValue();
                }
            } else if (mData instanceof List) {
                ((List) mData).remove(item);
                dispatchingValue();
            } else if (mData instanceof Set) {
                ((Set) mData).remove(item);
                dispatchingValue();
            }
        }
    }

    public void clear() {
        if (mData != null) {
            if (mData instanceof List) {
                ((List) mData).clear();
                dispatchingValue();
            } else if (mData instanceof Set) {
                ((Set) mData).clear();
                dispatchingValue();
            }
        }
    }

    public T get() {
        Object data = mData;
        if (data != DEFAULT_OBJECT) {
            return (T) data;
        }
        return null;
    }

    private void dispatchingValue() {
        for (int i = 0; i < mObservers.size(); i++) {
            mObservers.get(i).onChanged((T) mData);
        }
    }
}
