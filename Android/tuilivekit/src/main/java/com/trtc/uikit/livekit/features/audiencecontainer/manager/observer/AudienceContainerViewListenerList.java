package com.trtc.uikit.livekit.features.audiencecontainer.manager.observer;

import com.trtc.uikit.livekit.features.audiencecontainer.AudienceContainerViewDefine;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class AudienceContainerViewListenerList {
    private final List<WeakReference<AudienceContainerViewDefine.AudienceContainerViewListener>> mListeners =
            new CopyOnWriteArrayList<>();

    public void addListener(AudienceContainerViewDefine.AudienceContainerViewListener listener) {
        mListeners.add(new WeakReference<>(listener));
    }

    public void removeListener(AudienceContainerViewDefine.AudienceContainerViewListener listener) {
        for (WeakReference<AudienceContainerViewDefine.AudienceContainerViewListener> ref : mListeners) {
            if (ref.get() == listener) {
                mListeners.remove(ref);
            }
        }
    }

    public void clearListeners() {
        mListeners.clear();
    }

    public void notifyListeners(ListenerCallback callback) {
        List<WeakReference<AudienceContainerViewDefine.AudienceContainerViewListener>> listenersToRemove =
                new ArrayList<>();
        for (WeakReference<AudienceContainerViewDefine.AudienceContainerViewListener> ref : mListeners) {
            AudienceContainerViewDefine.AudienceContainerViewListener listener = ref.get();
            if (listener == null) {
                listenersToRemove.add(ref);
            } else {
                callback.onNotify(listener);
            }
        }
        mListeners.removeAll(listenersToRemove);
    }

    public interface ListenerCallback {
        void onNotify(AudienceContainerViewDefine.AudienceContainerViewListener listener);
    }
}
