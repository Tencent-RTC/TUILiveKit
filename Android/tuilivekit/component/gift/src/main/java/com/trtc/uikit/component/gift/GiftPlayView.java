package com.trtc.uikit.component.gift;

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
import com.trtc.uikit.component.gift.service.GiftConstants;
import com.trtc.uikit.component.gift.service.GiftPresenter;
import com.trtc.uikit.component.gift.store.GiftSendData;
import com.trtc.uikit.component.gift.store.GiftStore;
import com.trtc.uikit.component.gift.store.model.Gift;
import com.trtc.uikit.component.gift.store.model.GiftUser;
import com.trtc.uikit.component.gift.view.GiftBulletFrameLayout;
import com.trtc.uikit.component.gift.view.IGiftPlayView;
import com.trtc.uikit.component.gift.view.animation.AnimationView;
import com.trtc.uikit.component.gift.view.animation.manager.AnimationPlayer;
import com.trtc.uikit.component.gift.view.animation.manager.GiftAnimationManager;
import com.trtc.uikit.component.gift.view.like.GiftHeartLayout;

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

    private final GiftAnimationManager mGiftAnimationManager = new GiftAnimationManager();

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
        initView();
    }

    public void init(String roomId) {
        this.mRoomId = roomId;
        this.mAnimationView.setRoomId(mRoomId);
        initPresenter();
        initPlayer();
    }

    private void initView() {
        mGiftBulletGroup = findViewById(R.id.gift_bullet_group);
        mAnimationView = findViewById(R.id.gift_anim_view);
        mHeartLayout = findViewById(R.id.heart_layout);
    }

    private void initPlayer() {
        AnimationPlayer animationPlayer = new AnimationPlayer() {
            @Override
            public void preparePlay(Gift gift) {
                if (!isAttachedToWindow()) {
                    return;
                }
                if (mGiftPlayViewListener != null) {
                    mGiftPlayViewListener.onPlayGiftAnimation(GiftPlayView.this, gift);
                }
            }

            @Override
            public void startPlay(String url) {
                mAnimationView.playAnimation(url);
            }

            @Override
            public void stopPlay() {
                mAnimationView.stopPlay();
            }

            @Override
            public void setCallback(PlayCallback callback) {
                mAnimationView.setCallback(callback::onFinished);
            }
        };
        mGiftAnimationManager.setPlayer(animationPlayer);
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
        mGiftAnimationManager.stopPlay();
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
            GiftAnimationManager.GiftModel giftModel = new GiftAnimationManager.GiftModel();
            giftModel.gift = gift;
            giftModel.isFromSelf = sender != null && TextUtils.equals(sender.userId, TUILogin.getUserId());
            mGiftAnimationManager.add(giftModel);
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
        post(() -> mGiftAnimationManager.startPlay(playUrl));
    }

    public void setListener(TUIGiftPlayViewListener listener) {
        mGiftPlayViewListener = listener;
    }

    public interface TUIGiftPlayViewListener {
        void onReceiveGift(Gift gift, int giftCount, GiftUser sender, GiftUser receiver);

        void onPlayGiftAnimation(GiftPlayView view, Gift gift);
    }
}
