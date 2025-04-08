package com.trtc.uikit.livekit.component.gift.view;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ObjectAnimator;
import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.animation.DecelerateInterpolator;
import android.view.animation.OvershootInterpolator;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.view.animation.ImageAnimationView.GiftImageAnimationInfo;

/**
 * Items that are played as regular gifts
 */
public class GiftBulletFrameLayout extends FrameLayout {

    private static final int GIFT_DISMISS_TIME = 3000;

    private final Handler        mHandler = new Handler(Looper.getMainLooper());
    private final Context        mContext;
    private final LayoutInflater mLayoutInflater;

    private Runnable        mGiftEndAnimationRunnable;
    private RelativeLayout  mGiftGroup;
    private ImageFilterView mImageGiftIcon;
    private ImageView       mImageSendUserIcon;
    private TextView        mTextSendUserName;
    private TextView        mTextGiftTitle;
    private Callback        mCallback;

    private final GiftImageAnimationInfo mGiftImageAnimationInfo = new GiftImageAnimationInfo();

    public GiftBulletFrameLayout(Context context) {
        this(context, null);
    }

    public GiftBulletFrameLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        mLayoutInflater = LayoutInflater.from(context);
        mContext = context;
        initView();
    }

    private void initView() {
        View rootView = mLayoutInflater.inflate(R.layout.gift_layout_bullet, this);
        mGiftGroup = rootView.findViewById(R.id.gift_group);
        mImageGiftIcon = rootView.findViewById(R.id.iv_gift_icon);
        mImageSendUserIcon = rootView.findViewById(R.id.iv_send_user_icon);
        mTextSendUserName = rootView.findViewById(R.id.tv_send_user_name);
        mTextGiftTitle = rootView.findViewById(R.id.tv_gift_title);
        setVisibility(INVISIBLE);
    }

    public void setGiftInfo(GiftImageAnimationInfo info) {
        mGiftImageAnimationInfo.senderAvatarUrl = info.senderAvatarUrl;
        mGiftImageAnimationInfo.senderName = info.senderName;
        mGiftImageAnimationInfo.giftCount = info.giftCount;
        mGiftImageAnimationInfo.giftName = info.giftName;
        mGiftImageAnimationInfo.giftImageUrl = info.giftImageUrl;
        mTextSendUserName.setText(mGiftImageAnimationInfo.senderName);
        mTextGiftTitle.setText(mGiftImageAnimationInfo.giftName);
    }

    public void stopPlay() {
        setVisibility(INVISIBLE);
        mHandler.removeCallbacks(mGiftEndAnimationRunnable);
        mGiftImageAnimationInfo.reset();
    }

    private void initLayoutState() {
        if (!isAttachedToWindow()) {
            Log.w("GiftBulletFrameLayout", "initLayoutState: isAttachedToWindow is false");
            return;
        }
        this.setVisibility(View.VISIBLE);
        if (!TextUtils.isEmpty(mGiftImageAnimationInfo.giftImageUrl)) {
            ImageLoader.loadImage(mContext, mImageGiftIcon, mGiftImageAnimationInfo.giftImageUrl, R.drawable.gift_default_avatar);
        }
        if (!TextUtils.isEmpty(mGiftImageAnimationInfo.senderAvatarUrl)) {
            ImageLoader.loadImage(mContext, mImageSendUserIcon, mGiftImageAnimationInfo.senderAvatarUrl, R.drawable.gift_default_avatar);
        }
    }

    public void startAnimation() {
        setVisibility(View.VISIBLE);
        mImageGiftIcon.setVisibility(VISIBLE);
        final int duration = 400;
        ObjectAnimator giftLayoutAnimator = AnimationUtils.createFadesInFromLtoR(
                mGiftGroup, -getWidth(), 0, duration, new OvershootInterpolator());
        giftLayoutAnimator.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationStart(Animator animation) {
                initLayoutState();
            }
        });

        ObjectAnimator giftImageAnimator = AnimationUtils.createFadesInFromLtoR(
                mImageGiftIcon, -getWidth(), 0, duration, new DecelerateInterpolator());
        giftImageAnimator.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationStart(Animator animation) {
                mImageGiftIcon.setVisibility(VISIBLE);
            }
        });
        AnimationUtils.startAnimation(giftLayoutAnimator, giftImageAnimator);
        mGiftEndAnimationRunnable = this::endAnimation;
        mHandler.postDelayed(mGiftEndAnimationRunnable, GIFT_DISMISS_TIME);
    }

    public void endAnimation() {
        //The upward gradient disappears
        ObjectAnimator fadeAnimator = AnimationUtils.createFadesOutAnimator(
                GiftBulletFrameLayout.this, 0, -100, 500, 0);
        ObjectAnimator fadeAnimator2 = AnimationUtils.createFadesOutAnimator(
                GiftBulletFrameLayout.this, 100, 0, 0, 0);
        fadeAnimator2.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationEnd(Animator animation) {
                setVisibility(INVISIBLE);
                setAlpha(1);
                if (mCallback != null) {
                    mCallback.onFinished(0);
                }
            }
        });
        AnimationUtils.startAnimation(fadeAnimator, fadeAnimator2);
    }

    public void setCallback(Callback callback) {
        mCallback = callback;
    }

    public interface Callback {
        void onFinished(int error);
    }
}
