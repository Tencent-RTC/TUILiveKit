package com.trtc.uikit.livekit.features.anchorprepare.manager;

import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine.AnchorPrepareViewListener;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class AnchorPrepareListenerManager {
    private final List<WeakReference<AnchorPrepareViewListener>> mListenerList = new CopyOnWriteArrayList<>();

    public void addAnchorPrepareViewListener(AnchorPrepareViewListener listener) {
        mListenerList.add(new WeakReference<>(listener));
    }

    public void removeAnchorPrepareViewListener(AnchorPrepareViewListener listener) {
        for (WeakReference<AnchorPrepareViewListener> ref : mListenerList) {
            if (ref.get() == listener) {
                mListenerList.remove(ref);
            }
        }
    }

    public void clearAnchorPrepareViewListeners() {
        mListenerList.clear();
    }

    public void notifyAnchorPrepareViewListener(AnchorPrepareViewCallback callback) {
        List<WeakReference<AnchorPrepareViewListener>> observersToRemove = new ArrayList<>();
        for (WeakReference<AnchorPrepareViewListener> ref : mListenerList) {
            AnchorPrepareViewListener observer = ref.get();
            if (observer == null) {
                observersToRemove.add(ref);
            } else {
                callback.onNotify(observer);
            }
        }
        mListenerList.removeAll(observersToRemove);
    }

    public interface AnchorPrepareViewCallback {
        void onNotify(AnchorPrepareViewListener observer);
    }
}
