package com.tencent.qcloud.tuikit.tuigift.view;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import com.airbnb.lottie.LottieAnimationView;
import com.tencent.qcloud.tuikit.tuigift.R;
import com.tencent.qcloud.tuikit.tuigift.core.TUIGiftExtension;
import com.tencent.qcloud.tuikit.tuigift.model.TUIGiftModel;
import com.tencent.qcloud.tuikit.tuigift.presenter.TUIGiftPresenter;
import com.tencent.qcloud.tuikit.tuigift.view.like.TUIGiftHeartLayout;

import java.lang.ref.WeakReference;
import java.util.LinkedList;
import java.util.List;

/**
 * 礼物动画播放界面
 */
public class TUIGiftPlayView extends LinearLayout implements ITUIGiftPlayView {

    private static final String TAG                       = "TUIGiftPlayView";
    private static final int    MAX_SHOW_GIFT_BULLET_SIZE = 3; //礼物动画播放界面最多展示的个数

    private Context             mContext;
    private LinearLayout        mGiftBulletGroup;
    private TUIGiftPresenter    mPresenter;
    private String              mGroupId;
    private LottieAnimationView mAnimationView;
    private TUIGiftHeartLayout  mHeartLayout;
    private List<TUIGiftModel>  mSentGift;

    public TUIGiftPlayView(Context context) {
        super(context);
    }

    public TUIGiftPlayView(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        init();
    }

    public TUIGiftPlayView(Context context, String groupId) {
        this(context);
        this.mContext = context;
        this.mGroupId = groupId;
        mSentGift = new LinkedList<>();
        init();
    }

    /**
     * 初始化界面
     */
    private void init() {
        LayoutInflater.from(getContext()).inflate(R.layout.tuigift_layout_lottie_animator, this, true);
        mGiftBulletGroup = (LinearLayout) findViewById(R.id.gift_bullet_group);
        mAnimationView = findViewById(R.id.gift_lottie_view);
        mHeartLayout = findViewById(R.id.heart_layout);
        mAnimationView.addAnimatorListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationStart(Animator animation) {
                super.onAnimationStart(animation);
                mAnimationView.setVisibility(VISIBLE);
            }

            @Override
            public void onAnimationEnd(Animator animation) {
                super.onAnimationEnd(animation);
                mAnimationView.setVisibility(GONE);
                if (mSentGift.size() != 0) {
                    mAnimationView.setVisibility(VISIBLE);
                    mAnimationView.setAnimationFromUrl(mSentGift.get(0).animationUrl);
                    mSentGift.remove(0);
                    mAnimationView.playAnimation();
                }
            }
        });
        initPresenter();
        TUIGiftExtension.map.put(mGroupId + TUIGiftExtension.KEY_TYPE_PLAY, new WeakReference<Object>(this));
    }

    @Override
    protected void onDetachedFromWindow() {
        mPresenter.destroyPresenter();
        super.onDetachedFromWindow();
    }

    /**
     * 初始化Presenter
     */
    private void initPresenter() {
        mPresenter = new TUIGiftPresenter(mContext, mGroupId);
        mPresenter.initGiftPlayView(this);
    }

    /**
     * 礼物动画展示条
     *
     * @param model 待播放动画的礼物信息
     */
    public void showGiftBullet(TUIGiftModel model) {
        if (mGiftBulletGroup.getChildCount() >= MAX_SHOW_GIFT_BULLET_SIZE) {
            //如果礼物超过3个，就将第一个出现的礼物弹幕从界面上移除
            View firstShowBulletView = mGiftBulletGroup.getChildAt(0);
            if (firstShowBulletView != null) {
                TUIGiftBulletFrameLayout bulletView = (TUIGiftBulletFrameLayout) firstShowBulletView;
                bulletView.clearHandler();
                mGiftBulletGroup.removeView(bulletView);
            }
        }
        TUIGiftBulletFrameLayout giftFrameLayout = new TUIGiftBulletFrameLayout(mContext);
        mGiftBulletGroup.addView(giftFrameLayout);
        RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) mGiftBulletGroup.getLayoutParams();
        lp.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
        if (giftFrameLayout.setGift(model)) {
            giftFrameLayout.startAnimation();
        }
    }

    private void showGiftLottie() {
        if (mSentGift == null || mSentGift.size() == 0) {
            mAnimationView.setVisibility(GONE);
            return;
        }
        mAnimationView.setVisibility(VISIBLE);
        mAnimationView.setAnimationFromUrl(mSentGift.get(0).animationUrl);
        mSentGift.remove(0);
        mAnimationView.playAnimation();
    }

    /**
     * 接收礼物
     *
     * @param giftModel 收到的礼物信息
     */
    @Override
    public void receiveGift(final TUIGiftModel giftModel) {
        if (giftModel == null) {
            Log.d(TAG, "receiveGift data is empty");
            return;
        }
        if (!TextUtils.isEmpty(giftModel.animationUrl)) {
            mSentGift.add(giftModel);
            if (!mAnimationView.isAnimating()) {
                showGiftLottie();
            }

        } else {
            showGiftBullet(giftModel);
        }
    }

    public void receiveLike() {
        mHeartLayout.addFavor();
    }
}
