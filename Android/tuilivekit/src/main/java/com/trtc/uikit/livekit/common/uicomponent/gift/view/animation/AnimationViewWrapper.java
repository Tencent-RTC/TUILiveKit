package com.trtc.uikit.livekit.common.uicomponent.gift.view.animation;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public final class AnimationViewWrapper extends AnimationView {

    private final AnimationView mAnimationView;

    public AnimationViewWrapper(@NonNull Context context) {
        this(context, null);
    }

    public AnimationViewWrapper(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        mAnimationView = createAnimationView();
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        addView(mAnimationView, params);
    }

    private AnimationView createAnimationView() {
        TCEffectAnimationView animationView = new TCEffectAnimationView(getContext());
        if (animationView.getChildCount() > 0) {
            return animationView;
        } else {
            return new SVGAAnimationView(getContext());
        }
    }

    @Override
    public void playAnimation(String playUrl) {
        mAnimationView.playAnimation(playUrl);
    }

    @Override
    public void setCallback(Callback callback) {
        super.setCallback(callback);
        mAnimationView.setCallback(callback);
    }
}
