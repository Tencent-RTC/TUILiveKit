package com.trtc.uikit.livekit.component.gift.view.animation;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.LinearLayout;

import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.component.gift.view.GiftBulletFrameLayout;

public class ImageAnimationView extends LinearLayout {

    private final int CHILD_VIEW_COUNT = 3;

    private final GiftBulletFrameLayout[] mChildViews = new GiftBulletFrameLayout[CHILD_VIEW_COUNT];

    private Callback mCallback;

    public ImageAnimationView(Context context) {
        this(context, null);
    }

    public ImageAnimationView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        setOrientation(VERTICAL);
        for (int i = 0; i < mChildViews.length; i++) {
            GiftBulletFrameLayout bullet = new GiftBulletFrameLayout(getContext());
            bullet.setCallback(error -> checkFinished());
            mChildViews[i] = bullet;
            addView(bullet);
        }
    }

    public void playAnimation(GiftImageAnimationInfo model) {
        if (isAttachedToWindow()) {
            for (GiftBulletFrameLayout bullet : mChildViews) {
                if (bullet.getVisibility() == INVISIBLE) {
                    bullet.setGiftInfo(model);
                    bullet.startAnimation();
                    break;
                }
            }
            checkFinished();
        } else {
            postDelayed(this::checkFinished, 500);
        }
    }

    public void stopPlay() {
        for (GiftBulletFrameLayout bullet : mChildViews) {
            bullet.stopPlay();
        }
    }

    private void checkFinished() {
        for (GiftBulletFrameLayout bullet : mChildViews) {
            if (bullet.getVisibility() == INVISIBLE) {
                if (mCallback != null) {
                    mCallback.onFinished(0);
                }
                break;
            }
        }
    }

    public void setCallback(Callback callback) {
        mCallback = callback;
    }

    public interface Callback {
        void onFinished(int error);
    }

    public static class GiftImageAnimationInfo {
        public String senderAvatarUrl;
        public String senderName;
        public String giftName;
        public String giftImageUrl;
        public int    giftCount;

        public GiftImageAnimationInfo() {
            reset();
        }

        public void reset() {
            senderAvatarUrl = "";
            senderName = "";
            giftName = "";
            giftImageUrl = "";
            giftCount = 0;
        }
    }

}
