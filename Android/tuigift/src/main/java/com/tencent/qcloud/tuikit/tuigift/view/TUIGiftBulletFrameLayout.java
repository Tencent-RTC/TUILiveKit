package com.tencent.qcloud.tuikit.tuigift.view;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.AnimatorSet;
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

import com.tencent.qcloud.tuikit.tuigift.R;
import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftConstants;
import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;

import de.hdodenhof.circleimageview.CircleImageView;

/**
 * 普通礼物播放的Item
 */
public class TUIGiftBulletFrameLayout extends FrameLayout implements Handler.Callback {

    private static final String TAG = "TUIGiftBulletFrameLayout";

    private static final int MSG_START_ANIMATION = 1001;
    private static final int GIFT_DISMISS_TIME   = 3000; //礼物展示时间

    private Handler mHandler = new Handler(this);

    private Context         mContext;
    private LayoutInflater  mLayoutInflater;
    private Runnable        mGiftEndAnimationRunnable; //当前动画结束runnable
    private RelativeLayout  mGiftGroup;
    private View            mRootView;
    private CircleImageView mImageGiftIcon;
    private ImageView       mImageSendUserIcon;
    private TextView        mTextSendUserName;
    private TextView        mTextGiftTitle;
    private TUIGiftModel    mGiftModel;

    public TUIGiftBulletFrameLayout(Context context) {
        this(context, null);
    }

    public TUIGiftBulletFrameLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        mLayoutInflater = LayoutInflater.from(context);
        mContext = context;
        initView();
    }

    private void initView() {
        mRootView = mLayoutInflater.inflate(R.layout.tuigift_bullet, null);
        mGiftGroup = (RelativeLayout) mRootView.findViewById(R.id.gift_group);
        mImageGiftIcon = (CircleImageView) mRootView.findViewById(R.id.iv_gift_icon);
        mImageSendUserIcon = (ImageView) mRootView.findViewById(R.id.iv_send_user_icon);
        mTextSendUserName = (TextView) mRootView.findViewById(R.id.tv_send_user_name);
        mTextGiftTitle = (TextView) mRootView.findViewById(R.id.tv_gift_title);
        this.addView(mRootView);
    }

    public void hideView() {
        mImageGiftIcon.setVisibility(INVISIBLE);
    }

    /**
     * 设置待播放礼物
     *
     * @param giftModel 待播放礼物信息
     * @return 设置结果
     */
    public boolean setGift(TUIGiftModel giftModel) {
        if (giftModel == null) {
            return false;
        }
        mGiftModel = giftModel;
        if (!TextUtils.isEmpty(giftModel.extInfo.get(TUIGiftConstants.KEY_USER_NAME))) {
            mTextSendUserName.setText(giftModel.extInfo.get(TUIGiftConstants.KEY_USER_NAME));
        }
        if (!TextUtils.isEmpty(giftModel.giveDesc)) {
            String tip = String.format(mContext.getString(R.string.tuigift_send), giftModel.giveDesc);
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
        mGiftModel = null;
    }

    /**
     * 动画开始时调用
     */
    private void initLayoutState() {
        if (mGiftModel == null) {
            return;
        }
        this.setVisibility(View.VISIBLE);
        TUIImageLoader.loadImage(mContext, mImageSendUserIcon, mGiftModel.extInfo.get(TUIGiftConstants.KEY_USER_AVATAR),
                R.drawable.tuigift_ic_head);
        TUIImageLoader.loadImage(mContext, mImageGiftIcon, mGiftModel.normalImageUrl, R.drawable.tuigift_ic_head);
    }

    private void startAnimationForMsg() {
        hideView();
        ObjectAnimator giftLayoutAnimator = TUIAnimationUtils.createFadesInFromLtoR(
                mGiftGroup, -getWidth(), 0, 400, new OvershootInterpolator());
        giftLayoutAnimator.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationStart(Animator animation) {
                super.onAnimationStart(animation);
                initLayoutState();
            }
        });

        ObjectAnimator giftImageAnimator = TUIAnimationUtils.createFadesInFromLtoR(
                mImageGiftIcon, -getWidth(), 0, 400, new DecelerateInterpolator());
        giftImageAnimator.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationStart(Animator animation) {
                mImageGiftIcon.setVisibility(View.VISIBLE);
            }

            @Override
            public void onAnimationEnd(Animator animation) {
            }
        });
        TUIAnimationUtils.startAnimation(giftLayoutAnimator, giftImageAnimator);
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

    public AnimatorSet endAnimation() {
        //向上渐变消失
        ObjectAnimator fadeAnimator = TUIAnimationUtils.createFadesOutAnimator(
                TUIGiftBulletFrameLayout.this, 0, -100, 500, 0);
        fadeAnimator.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationEnd(Animator animation) {
                //动画结束之后在父容器中移除自己
                ViewGroup viewGroup = (ViewGroup) getParent();
                if (viewGroup != null) {
                    viewGroup.removeView(TUIGiftBulletFrameLayout.this);
                }
            }
        });
        ObjectAnimator fadeAnimator2 = TUIAnimationUtils.createFadesOutAnimator(
                TUIGiftBulletFrameLayout.this, 100, 0, 0, 0);
        AnimatorSet animatorSet = TUIAnimationUtils.startAnimation(fadeAnimator, fadeAnimator2);
        return animatorSet;
    }
}
