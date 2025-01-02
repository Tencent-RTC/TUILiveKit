package com.trtc.uikit.component.gift.view.animation.manager;

import com.trtc.uikit.component.gift.store.model.Gift;

public abstract class AnimationPlayer {
    public abstract void preparePlay(Gift gift);

    public abstract void startPlay(String url);

    public abstract void stopPlay();

    public abstract void setCallback(PlayCallback callback);

    public interface PlayCallback {
        void onFinished(int error);
    }
}