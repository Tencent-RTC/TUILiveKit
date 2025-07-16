package com.trtc.uikit.livekit.features.anchorboardcast.manager.module;

import com.trtc.uikit.livekit.features.anchorboardcast.AnchorViewDefine.AnchorViewListener;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class AnchorViewListenerManager {
    private final List<WeakReference<AnchorViewListener>> mListenerList = new CopyOnWriteArrayList<>();

    public void addAnchorViewListener(AnchorViewListener listener) {
        mListenerList.add(new WeakReference<>(listener));
    }

    public void removeAnchorViewListener(AnchorViewListener listener) {
        for (WeakReference<AnchorViewListener> ref : mListenerList) {
            if (ref.get() == listener) {
                mListenerList.remove(ref);
            }
        }
    }

    public void clearAnchorViewListeners() {
        mListenerList.clear();
    }

    public void notifyAnchorViewListener(AnchorViewCallback callback) {
        List<WeakReference<AnchorViewListener>> observersToRemove = new ArrayList<>();
        for (WeakReference<AnchorViewListener> ref : mListenerList) {
            AnchorViewListener observer = ref.get();
            if (observer == null) {
                observersToRemove.add(ref);
            } else {
                callback.onNotify(observer);
            }
        }
        mListenerList.removeAll(observersToRemove);
    }

    public interface AnchorViewCallback {
        void onNotify(AnchorViewListener observer);
    }
}
