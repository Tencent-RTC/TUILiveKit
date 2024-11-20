package com.trtc.uikit.livekit.component.gift.view.animation;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public abstract class AnimationView extends FrameLayout {
    protected Callback mCallback;

    public AnimationView(@NonNull Context context) {
        this(context, null);
    }

    public AnimationView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public abstract void playAnimation(String playUrl);

    public void setCallback(Callback callback) {
        mCallback = callback;
    }

    public interface Callback {
        void onFinished(int error);
    }
}
