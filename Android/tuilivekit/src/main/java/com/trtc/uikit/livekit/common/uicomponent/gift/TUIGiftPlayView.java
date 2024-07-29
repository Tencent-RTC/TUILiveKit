package com.trtc.uikit.livekit.common.uicomponent.gift;

import android.annotation.SuppressLint;
import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import androidx.core.util.Pair;

import com.tencent.qcloud.tuicore.TUICore;
import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.ITUINotification;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGift;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftConstants;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftPresenter;
import com.trtc.uikit.livekit.common.uicomponent.gift.store.GiftSendData;
import com.trtc.uikit.livekit.common.uicomponent.gift.store.GiftStore;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.GiftBulletFrameLayout;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.IGiftPlayView;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.animation.AnimationView;
import com.trtc.uikit.livekit.common.uicomponent.gift.view.like.GiftHeartLayout;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

import java.util.LinkedList;
import java.util.List;

@SuppressLint("ViewConstructor")
public class TUIGiftPlayView extends LinearLayout implements IGiftPlayView {

    private static final String TAG                       = "TUIGiftPlayView";
    private static final int    MAX_SHOW_GIFT_BULLET_SIZE = 3;

    private final Context                              mContext;
    private       LinearLayout                         mGiftBulletGroup;
    private       GiftPresenter                        mPresenter;
    private final String                               mRoomId;
    private       AnimationView                        mAnimationView;
    private       GiftHeartLayout                      mHeartLayout;
    private final List<TUIGift>                        mSentGiftAnimation;
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

    public TUIGiftPlayView(Context context, String roomId) {
        super(context);
        this.mContext = context;
        this.mRoomId = roomId;
        mSentGiftAnimation = new LinkedList<>();
        init();
    }

    private void init() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_gift_layout_animator, this, true);
        mGiftBulletGroup = findViewById(R.id.gift_bullet_group);
        mAnimationView = findViewById(R.id.gift_anim_view);
        mHeartLayout = findViewById(R.id.heart_layout);

        mAnimationView.setCallback(error -> {
            if (error != 0) {
                LiveKitLog.error(TAG + " onPlayError:" + error);
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
        mPresenter.destroyPresenter();
        super.onDetachedFromWindow();
    }

    private void initPresenter() {
        mPresenter = new GiftPresenter(mRoomId);
        mPresenter.initGiftPlayView(this);
    }

    private void showGiftBullet(TUIGift model, TUIGiftUser sender, TUIGiftUser receiver, int giftCount) {
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
    public void receiveGift(final TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver) {
        if (gift == null) {
            LiveKitLog.error(TAG + " receiveGift data is empty");
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
        LiveKitLog.info(TAG + " playGiftAnimation playUrl = " + playUrl);
        post(() -> mAnimationView.playAnimation(playUrl));
    }

    public void setListener(TUIGiftPlayViewListener listener) {
        mGiftPlayViewListener = listener;
    }

    public interface TUIGiftPlayViewListener {
        void onReceiveGift(TUIGift gift, int giftCount, TUIGiftUser sender, TUIGiftUser receiver);

        void onPlayGiftAnimation(TUIGiftPlayView view, TUIGift gift);
    }
}
