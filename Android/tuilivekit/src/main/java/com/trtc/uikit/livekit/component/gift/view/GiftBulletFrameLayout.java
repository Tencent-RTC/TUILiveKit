package com.trtc.uikit.livekit.component.gift.view;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ObjectAnimator;
import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.DecelerateInterpolator;
import android.view.animation.OvershootInterpolator;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;

/**
 * Items that are played as regular gifts
 */
public class GiftBulletFrameLayout extends FrameLayout implements Handler.Callback {

    private static final int MSG_START_ANIMATION = 1001;
    private static final int GIFT_DISMISS_TIME   = 3000;

    private       Handler        mHandler = new Handler(this);
    private final Context        mContext;
    private final LayoutInflater mLayoutInflater;

    private Runnable        mGiftEndAnimationRunnable;
    private RelativeLayout  mGiftGroup;
    private ImageFilterView mImageGiftIcon;
    private ImageView       mImageSendUserIcon;
    private TextView        mTextSendUserName;
    private TextView        mTextGiftTitle;
    private Gift            mGift;
    private GiftUser        mSender;

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
        View rootView = mLayoutInflater.inflate(R.layout.livekit_gift_bullet, this);
        mGiftGroup = (RelativeLayout) rootView.findViewById(R.id.gift_group);
        mImageGiftIcon = (ImageFilterView) rootView.findViewById(R.id.iv_gift_icon);
        mImageSendUserIcon = (ImageView) rootView.findViewById(R.id.iv_send_user_icon);
        mTextSendUserName = (TextView) rootView.findViewById(R.id.tv_send_user_name);
        mTextGiftTitle = (TextView) rootView.findViewById(R.id.tv_gift_title);
        setVisibility(INVISIBLE);
    }

    public void hideView() {
        mImageGiftIcon.setVisibility(INVISIBLE);
    }

    public boolean setGift(Gift gift, int giftCount, GiftUser sender, GiftUser receiver) {
        if (gift == null) {
            return false;
        }
        mGift = gift;
        mSender = sender;
        if (!TextUtils.isEmpty(sender.userName)) {
            mTextSendUserName.setText(sender.userName);
        }
        if (!TextUtils.isEmpty(gift.giftName)) {
            String tip = String.format("%s%s%sx%s", mContext.getString(R.string.livekit_sent),
                    receiver.userName, gift.giftName, giftCount);
            mTextGiftTitle.setText(tip);
        }
        return true;
    }

    @Override
    public boolean handleMessage(Message msg) {
        if (msg.what == MSG_START_ANIMATION) {
            startAnimationForMsg();
        }
        return true;
    }

    public void clearHandler() {
        if (mHandler != null) {
            mHandler.removeCallbacksAndMessages(null);
            mHandler = null;
        }
        resetGift();
    }

    public void resetGift() {
        mGiftEndAnimationRunnable = null;
        mGift = null;
    }

    private void initLayoutState() {
        if (mGift != null) {
            ImageLoader.loadImage(mContext, mImageGiftIcon, mGift.imageUrl, R.drawable.livekit_ic_avatar);
        }
        this.setVisibility(View.VISIBLE);
        if (mSender != null) {
            ImageLoader.loadImage(mContext, mImageSendUserIcon, mSender.avatarUrl,
                    R.drawable.livekit_ic_avatar);
        }
    }

    private void startAnimationForMsg() {
        hideView();
        ObjectAnimator giftLayoutAnimator = AnimationUtils.createFadesInFromLtoR(
                mGiftGroup, -getWidth(), 0, 400, new OvershootInterpolator());
        giftLayoutAnimator.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationStart(Animator animation) {
                super.onAnimationStart(animation);
                initLayoutState();
            }
        });

        ObjectAnimator giftImageAnimator = AnimationUtils.createFadesInFromLtoR(
                mImageGiftIcon, -getWidth(), 0, 400, new DecelerateInterpolator());
        giftImageAnimator.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationStart(Animator animation) {
                mImageGiftIcon.setVisibility(View.VISIBLE);
            }
        });
        AnimationUtils.startAnimation(giftLayoutAnimator, giftImageAnimator);
        mGiftEndAnimationRunnable = new GiftEndAnimationRunnable();
        mHandler.postDelayed(mGiftEndAnimationRunnable, GIFT_DISMISS_TIME);
    }

    public void startAnimation() {
        mHandler.sendEmptyMessage(MSG_START_ANIMATION);
    }

    private class GiftEndAnimationRunnable implements Runnable {

        @Override
        public void run() {
            endAnimation();
        }
    }

    public void endAnimation() {
        //The upward gradient disappears
        ObjectAnimator fadeAnimator = AnimationUtils.createFadesOutAnimator(
                GiftBulletFrameLayout.this, 0, -100, 500, 0);
        fadeAnimator.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationEnd(Animator animation) {
                ViewGroup viewGroup = (ViewGroup) getParent();
                if (viewGroup != null) {
                    viewGroup.removeView(GiftBulletFrameLayout.this);
                }
            }
        });
        ObjectAnimator fadeAnimator2 = AnimationUtils.createFadesOutAnimator(
                GiftBulletFrameLayout.this, 100, 0, 0, 0);
        AnimationUtils.startAnimation(fadeAnimator, fadeAnimator2);
    }
}
