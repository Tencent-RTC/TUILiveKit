package com.trtc.uikit.livekit.component.gift;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import androidx.core.util.Pair;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.service.GiftConstants;
import com.trtc.uikit.livekit.component.gift.service.GiftPresenter;
import com.trtc.uikit.livekit.component.gift.store.GiftSendData;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.gift.view.GiftBulletFrameLayout;
import com.trtc.uikit.livekit.component.gift.view.IGiftPlayView;
import com.trtc.uikit.livekit.component.gift.view.animation.AnimationView;
import com.trtc.uikit.livekit.component.gift.view.like.GiftHeartLayout;

import java.util.LinkedList;
import java.util.List;

@SuppressLint("ViewConstructor")
public class GiftPlayView extends FrameLayout implements IGiftPlayView {

    private static final String TAG                       = "GiftPlayView";
    private static final int    MAX_SHOW_GIFT_BULLET_SIZE = 3;

    private final Context                              mContext;
    private       LinearLayout                         mGiftBulletGroup;
    private       GiftPresenter                        mPresenter;
    private       String                               mRoomId;
    private       AnimationView                        mAnimationView;
    private       GiftHeartLayout                      mHeartLayout;
    private       List<Gift>                           mSentGiftAnimation;
    private       TUIGiftPlayViewListener              mGiftPlayViewListener;
    private       int                                  mLikeCount            = 0;
    private final ITUINotification                     mLikeNotification     = (key, subKey, param) -> receiveLike();
    private final Observer<Pair<String, GiftSendData>> mGiftSendDataObserver =
            new Observer<Pair<String, GiftSendData>>() {
                @Override
                public void onChanged(Pair<String, GiftSendData> pair) {
                    if (pair != null && TextUtils.equals(pair.first, mRoomId)) {
                        GiftSendData data = pair.second;
                        receiveGift(data.gift, data.giftCount, data.sender, data.receiver);
                    }
                }
            };

    public GiftPlayView(Context context) {
        this(context, null);
    }

    public GiftPlayView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public GiftPlayView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_gift_layout_animator, this, true);
    }

    public void init(String roomId) {
        this.mRoomId = roomId;
        mSentGiftAnimation = new LinkedList<>();
        initView();
    }

    private void initView() {
        mGiftBulletGroup = findViewById(R.id.gift_bullet_group);
        mAnimationView = findViewById(R.id.gift_anim_view);
        mHeartLayout = findViewById(R.id.heart_layout);

        mAnimationView.setCallback(error -> {
            if (error != 0) {
                Log.i(TAG, "onPlayError:" + error);
            }
            if (isAttachedToWindow()) {
                mSentGiftAnimation.remove(0);
                showGiftAnimation();
            }
        });
        initPresenter();
    }

    @Override
    protected void onAttachedToWindow() {
        TUICore.registerEvent(GiftConstants.KEY_LIKE, GiftConstants.SUB_KEY_LIKE_SEND, mLikeNotification);
        GiftStore.getInstance().mGiftSendData.set(null);
        GiftStore.getInstance().mGiftSendData.observe(mGiftSendDataObserver);
        super.onAttachedToWindow();
    }

    @Override
    protected void onDetachedFromWindow() {
        TUICore.unRegisterEvent(GiftConstants.KEY_LIKE, GiftConstants.SUB_KEY_LIKE_SEND, mLikeNotification);
        GiftStore.getInstance().mGiftSendData.removeObserver(mGiftSendDataObserver);
        GiftStore.getInstance().mGiftSendData.set(null);
        if (mPresenter != null) {
            mPresenter.destroyPresenter();
        }
        super.onDetachedFromWindow();
    }

    private void initPresenter() {
        mPresenter = new GiftPresenter(mRoomId);
        mPresenter.initGiftPlayView(this);
    }

    private void showGiftBullet(Gift model, GiftUser sender, GiftUser receiver, int giftCount) {
        if (mGiftBulletGroup.getChildCount() >= MAX_SHOW_GIFT_BULLET_SIZE) {
            //If there are more than 3 gifts, the first gift barrage will be removed from the interface
            View firstShowBulletView = mGiftBulletGroup.getChildAt(0);
            if (firstShowBulletView != null) {
                GiftBulletFrameLayout bulletView = (GiftBulletFrameLayout) firstShowBulletView;
                bulletView.clearHandler();
                mGiftBulletGroup.removeView(bulletView);
            }
        }
        GiftBulletFrameLayout giftFrameLayout = new GiftBulletFrameLayout(mContext);
        mGiftBulletGroup.addView(giftFrameLayout);
        RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) mGiftBulletGroup.getLayoutParams();
        lp.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
        if (giftFrameLayout.setGift(model, giftCount, sender, receiver)) {
            giftFrameLayout.startAnimation();
        }
    }

    private void showGiftAnimation() {
        if (!mSentGiftAnimation.isEmpty() && mGiftPlayViewListener != null) {
            mGiftPlayViewListener.onPlayGiftAnimation(this, mSentGiftAnimation.get(0));
        }
    }

    /**
     * Insert a gift record (local sending/receiving gifts from the remote will pass through)
     */
    @Override
    public void receiveGift(final Gift gift, int giftCount, GiftUser sender, GiftUser receiver) {
        if (gift == null) {
            Log.i(TAG, "receiveGift data is empty");
            return;
        }
        if (sender != null && TextUtils.equals(sender.userId, TUILogin.getUserId())) {
            sender.userName = mContext.getString(R.string.livekit_gift_me);
        }
        if (receiver != null && TextUtils.equals(receiver.userId, TUILogin.getUserId())) {
            receiver.userName = mContext.getString(R.string.livekit_gift_me);
        }
        if (TextUtils.isEmpty(gift.animationUrl)) {
            showGiftBullet(gift, sender, receiver, giftCount);
        } else {
            mSentGiftAnimation.add(gift);
            if (mSentGiftAnimation.size() == 1) {
                showGiftAnimation();
            }
        }
        if (mGiftPlayViewListener != null) {
            mGiftPlayViewListener.onReceiveGift(gift, giftCount, sender, receiver);
        }
    }

    public void receiveLike() {
        mLikeCount++;
        mHeartLayout.addFavor();
    }

    public int getLikeCount() {
        return mLikeCount;
    }

    public void playGiftAnimation(String playUrl) {
        Log.i(TAG, "playGiftAnimation playUrl = " + playUrl);
        post(() -> mAnimationView.playAnimation(playUrl));
    }

    public void setListener(TUIGiftPlayViewListener listener) {
        mGiftPlayViewListener = listener;
    }

    public interface TUIGiftPlayViewListener {
        void onReceiveGift(Gift gift, int giftCount, GiftUser sender, GiftUser receiver);

        void onPlayGiftAnimation(GiftPlayView view, Gift gift);
    }
}
