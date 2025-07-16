package com.trtc.uikit.livekit.features.audiencecontainer.manager.observer;

import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class AudienceViewListenerList {
    private final List<WeakReference<AudienceManager.AudienceViewListener>> mListeners =
            new CopyOnWriteArrayList<>();

    public void addListener(AudienceManager.AudienceViewListener listener) {
        mListeners.add(new WeakReference<>(listener));
    }

    public void removeListener(AudienceManager.AudienceViewListener listener) {
        for (WeakReference<AudienceManager.AudienceViewListener> ref : mListeners) {
            if (ref.get() == listener) {
                mListeners.remove(ref);
            }
        }
    }

    public void clearListeners() {
        mListeners.clear();
    }

    public void notifyListeners(ListenerCallback callback) {
        List<WeakReference<AudienceManager.AudienceViewListener>> observersToRemove =
                new ArrayList<>();
        for (WeakReference<AudienceManager.AudienceViewListener> ref : mListeners) {
            AudienceManager.AudienceViewListener listener = ref.get();
            if (listener == null) {
                observersToRemove.add(ref);
            } else {
                callback.onNotify(listener);
            }
        }
        mListeners.removeAll(observersToRemove);
    }

    public interface ListenerCallback {
        void onNotify(AudienceManager.AudienceViewListener listener);
    }
}
