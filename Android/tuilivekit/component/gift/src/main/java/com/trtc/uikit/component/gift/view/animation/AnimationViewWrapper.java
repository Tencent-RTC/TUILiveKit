package com.trtc.uikit.component.gift.view.animation;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public final class AnimationViewWrapper extends AnimationView {

    private static final String TAG = "AnimationViewWrapper";

    private enum AnimationSourceType {
        SVGA, MP4, OTHER
    }

    private AnimationView         mAnimationView;
    private TCEffectAnimationView mEffectAnimationView;
    private SVGAAnimationView     mSVGAAnimationView;

    public AnimationViewWrapper(@NonNull Context context) {
        this(context, null);
    }

    public AnimationViewWrapper(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    @Override
    public void playAnimation(String playUrl) {
        AnimationSourceType sourceType = getSourceType(playUrl);
        if (sourceType == AnimationSourceType.MP4) {
            playEffectAnimation(playUrl);
        } else if (sourceType == AnimationSourceType.SVGA) {
            playSVGAAnimation(playUrl);
        } else {
            Log.e(TAG, "not support url:" + playUrl);
            if (mCallback != null) {
                mCallback.onFinished(-1);
            }
        }
    }

    private void playEffectAnimation(String url) {
        if (mEffectAnimationView == null) {
            mEffectAnimationView = new TCEffectAnimationView(getContext());
        }
        if (mEffectAnimationView.getChildCount() > 0) {
            if (mAnimationView != mEffectAnimationView) {
                switchAnimationView(mEffectAnimationView);
                mAnimationView = mEffectAnimationView;
            }
            mAnimationView.setCallback(mCallback);
            mAnimationView.playAnimation(url);
        } else {
            Log.e(TAG, "not support TCEffectAnimationView");
            if (mCallback != null) {
                mCallback.onFinished(-1);
            }
        }
    }

    private void playSVGAAnimation(String url) {
        if (mSVGAAnimationView == null) {
            mSVGAAnimationView = new SVGAAnimationView(getContext());
        }
        if (mAnimationView != mSVGAAnimationView) {
            switchAnimationView(mSVGAAnimationView);
            mAnimationView = mSVGAAnimationView;
        }
        mAnimationView.setCallback(mCallback);
        mAnimationView.playAnimation(url);
    }

    private void switchAnimationView(AnimationView view) {
        removeAllViews();
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        addView(view, params);
    }

    private AnimationSourceType getSourceType(String url) {
        if (TextUtils.isEmpty(url)) {
            return AnimationSourceType.OTHER;
        }
        if (url.toLowerCase().endsWith(".mp4")) {
            return AnimationSourceType.MP4;
        } else if (url.toLowerCase().endsWith(".svga")) {
            return AnimationSourceType.SVGA;
        }
        return AnimationSourceType.OTHER;
    }

    @Override
    public void stopPlay() {
        if (mAnimationView != null) {
            mAnimationView.stopPlay();
        }
    }
}
