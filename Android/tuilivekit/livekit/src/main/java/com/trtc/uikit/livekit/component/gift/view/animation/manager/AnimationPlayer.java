package com.trtc.uikit.livekit.component.gift.view.animation.manager;

public abstract class AnimationPlayer {
    public abstract void preparePlay(GiftAnimationModel model);

    public abstract void startPlay(GiftAnimationModel model);

    public abstract void stopPlay();

    public abstract void setCallback(PlayCallback callback);

    public interface PlayCallback {
        void onFinished(int error);
    }
}