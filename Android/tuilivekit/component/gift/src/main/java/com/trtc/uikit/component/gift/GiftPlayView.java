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

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.component.gift.store.GiftSendData;
import com.trtc.uikit.component.gift.store.GiftState;
import com.trtc.uikit.component.gift.store.GiftStore;
import com.trtc.uikit.component.gift.store.LikeState;
import com.trtc.uikit.component.gift.store.model.Gift;
import com.trtc.uikit.component.gift.store.model.GiftUser;
import com.trtc.uikit.component.gift.view.GiftBulletFrameLayout;
import com.trtc.uikit.component.gift.view.animation.AnimationView;
import com.trtc.uikit.component.gift.view.animation.manager.AnimationPlayer;
import com.trtc.uikit.component.gift.view.animation.manager.GiftAnimationManager;
import com.trtc.uikit.component.gift.view.like.GiftHeartLayout;

import java.util.List;

@SuppressLint("ViewConstructor")
public class GiftPlayView extends FrameLayout {

    private static final String TAG                       = "GiftPlayView";
    private static final int    MAX_SHOW_GIFT_BULLET_SIZE = 3;

    private final Context                      mContext;
    private       LinearLayout                 mGiftBulletGroup;
    private       String                       mRoomId;
    private       AnimationView                mAnimationView;
    private       GiftHeartLayout              mHeartLayout;
    private       TUIGiftPlayViewListener      mGiftPlayViewListener;
    private final Observer<Boolean>            mLikeObserver     = this::onLikeTriggered;
    private final Observer<List<GiftSendData>> mGiftListObserver = this::onGiftListChanged;
    private       GiftState                    mGiftState;
    private       LikeState                    mLikeState;

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
        initPlayer();
        initLikeState();
        if (isAttachedToWindow()) {
            initGiftState();
        }
    }

    private void initView() {
        mGiftBulletGroup = findViewById(R.id.gift_bullet_group);
        mAnimationView = findViewById(R.id.gift_anim_view);
        mHeartLayout = findViewById(R.id.heart_layout);
    }

    private void initGiftState() {
        if (mGiftState != null || TextUtils.isEmpty(mRoomId)) {
            return;
        }
        mGiftState = GiftStore.sharedInstance().getGiftState(mRoomId);
        mGiftState.mGiftCacheList.observe(mGiftListObserver);
    }

    private void initLikeState() {
        if (mLikeState != null || TextUtils.isEmpty(mRoomId)) {
            return;
        }
        mLikeState = GiftStore.sharedInstance().getLikeState(mRoomId);
        mLikeState.mLikeAnimationTrigger.observe(mLikeObserver);
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
        super.onAttachedToWindow();
        initGiftState();
    }

    @Override
    protected void onDetachedFromWindow() {
        if (mGiftState != null) {
            mGiftState.mGiftCacheList.removeObserver(mGiftListObserver);
        }
        if (mLikeState != null) {
            mLikeState.mLikeAnimationTrigger.removeObserver(mLikeObserver);
        }
        mGiftAnimationManager.stopPlay();
        super.onDetachedFromWindow();
    }

    public int getLikeCount() {
        return mLikeState == null ? 0 : mLikeState.mLikeReceivedTotalCount.get();
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

    private void onGiftListChanged(List<GiftSendData> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        for (GiftSendData data : list) {
            receiveGift(data.gift, data.giftCount, data.sender, data.receiver);
        }
        GiftStore.sharedInstance().mGiftIMService.cleanGiftCacheList(mRoomId);
    }

    /**
     * Insert a gift record (local sending/receiving gifts from the remote will pass through)
     */
    private void receiveGift(final Gift gift, int giftCount, GiftUser sender, GiftUser receiver) {
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
        if (giftFrameLayout.setGift(model, giftCount, sender, receiver)) {
            giftFrameLayout.startAnimation();
        }
    }

    private void onLikeTriggered(Boolean triggered) {
        if (Boolean.TRUE.equals(triggered)) {
            mHeartLayout.addFavor();
        }
    }
}
