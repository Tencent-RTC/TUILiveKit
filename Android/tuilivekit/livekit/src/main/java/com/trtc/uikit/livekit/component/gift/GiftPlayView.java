package com.trtc.uikit.livekit.component.gift;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Looper;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.widget.FrameLayout;

import androidx.lifecycle.Observer;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.store.GiftSendData;
import com.trtc.uikit.livekit.component.gift.store.GiftState;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;
import com.trtc.uikit.livekit.component.gift.store.LikeState;
import com.trtc.uikit.livekit.component.gift.store.model.Gift;
import com.trtc.uikit.livekit.component.gift.store.model.GiftUser;
import com.trtc.uikit.livekit.component.gift.view.animation.AnimationView;
import com.trtc.uikit.livekit.component.gift.view.animation.ImageAnimationView;
import com.trtc.uikit.livekit.component.gift.view.animation.manager.AnimationPlayer;
import com.trtc.uikit.livekit.component.gift.view.animation.manager.GiftAnimationManager;
import com.trtc.uikit.livekit.component.gift.view.animation.manager.GiftAnimationModel;
import com.trtc.uikit.livekit.component.gift.view.like.GiftHeartLayout;

import java.util.List;

@SuppressLint("ViewConstructor")
public class GiftPlayView extends FrameLayout {

    private static final String TAG = "GiftPlayView";

    private final Context                      mContext;
    private       String                       mRoomId;
    private       ImageAnimationView           mImageAnimationView;
    private       AnimationView                mAnimationView;
    private       GiftHeartLayout              mHeartLayout;
    private       TUIGiftPlayViewListener      mGiftPlayViewListener;
    private final Observer<Boolean>            mLikeObserver     = this::onLikeTriggered;
    private final Observer<List<GiftSendData>> mGiftListObserver = this::onGiftListChanged;
    private       GiftState                    mGiftState;
    private       LikeState                    mLikeState;

    private final GiftAnimationManager mGiftAnimationManager      = new GiftAnimationManager();
    private final GiftAnimationManager mGiftImageAnimationManager = new GiftAnimationManager();

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
        initAnimationPlayer();
        initImageAnimationPlayer();
    }

    public void init(String roomId) {
        this.mRoomId = roomId;
        this.mAnimationView.setRoomId(mRoomId);
        GiftStore.sharedInstance().init(roomId);
        initLikeState();
        initGiftState();
    }

    private void initView() {
        mImageAnimationView = findViewById(R.id.gift_image_anim_view);
        mAnimationView = findViewById(R.id.gift_anim_view);
        mHeartLayout = findViewById(R.id.heart_layout);
    }

    private void initGiftState() {
        if (TextUtils.isEmpty(mRoomId)) {
            return;
        }
        mGiftState = GiftStore.sharedInstance().getGiftState(mRoomId);
        mGiftState.mGiftCacheList.observeForever(mGiftListObserver);
    }

    private void initLikeState() {
        if (TextUtils.isEmpty(mRoomId)) {
            return;
        }
        mLikeState = GiftStore.sharedInstance().getLikeState(mRoomId);
        mLikeState.mLikeAnimationTrigger.observeForever(mLikeObserver);
    }

    private void initAnimationPlayer() {
        AnimationPlayer animationPlayer = new AnimationPlayer() {
            @Override
            public void preparePlay(GiftAnimationModel model) {
                if (isAttachedToWindow()) {
                    if (mGiftPlayViewListener != null) {
                        mGiftPlayViewListener.onPlayGiftAnimation(GiftPlayView.this, model.gift);
                    }
                }
            }

            @Override
            public void startPlay(GiftAnimationModel model) {
                if (isAttachedToWindow()) {
                    mAnimationView.playAnimation(model.gift.animationUrl);
                }
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

    private void initImageAnimationPlayer() {
        AnimationPlayer imageAnimationPlayer = new AnimationPlayer() {

            @Override
            public void preparePlay(GiftAnimationModel model) {
                if (isAttachedToWindow()) {
                    mGiftImageAnimationManager.startPlay(model);
                }
            }

            @Override
            public void startPlay(GiftAnimationModel model) {
                if (isAttachedToWindow()) {
                    ImageAnimationView.GiftImageAnimationInfo info = new ImageAnimationView.GiftImageAnimationInfo();
                    info.giftImageUrl = model.gift.imageUrl;
                    info.giftName = model.gift.giftName;
                    info.giftCount = model.giftCount;
                    info.senderName = model.sender.userName;
                    info.senderAvatarUrl = model.sender.avatarUrl;
                    mImageAnimationView.playAnimation(info);
                }
            }

            @Override
            public void stopPlay() {
                mImageAnimationView.stopPlay();
            }

            @Override
            public void setCallback(PlayCallback callback) {
                mImageAnimationView.setCallback(callback::onFinished);
            }
        };
        mGiftImageAnimationManager.setPlayer(imageAnimationPlayer);
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
        mGiftImageAnimationManager.stopPlay();
        super.onDetachedFromWindow();
    }

    public int getLikeCount() {
        return mLikeState == null ? 0 : mLikeState.mLikeReceivedTotalCount.getValue();
    }

    public void playGiftAnimation(String playUrl) {
        GiftAnimationModel model = new GiftAnimationModel();
        model.gift = new Gift();
        model.gift.animationUrl = playUrl;
        if (Looper.myLooper() == Looper.getMainLooper()) {
            mGiftAnimationManager.startPlay(model);
        } else {
            post(() -> mGiftAnimationManager.startPlay(model));
        }
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
            sender.userName = mContext.getString(R.string.live_gift_me);
        }
        if (receiver != null && TextUtils.equals(receiver.userId, TUILogin.getUserId())) {
            receiver.userName = mContext.getString(R.string.live_gift_me);
        }
        GiftAnimationModel giftModel = new GiftAnimationModel();
        giftModel.gift = gift;
        giftModel.giftCount = giftCount;
        giftModel.sender = sender;
        giftModel.isFromSelf = sender != null && TextUtils.equals(sender.userId, TUILogin.getUserId());
        if (TextUtils.isEmpty(gift.animationUrl)) {
            mGiftImageAnimationManager.add(giftModel);
        } else {
            mGiftAnimationManager.add(giftModel);
        }
        if (mGiftPlayViewListener != null) {
            mGiftPlayViewListener.onReceiveGift(gift, giftCount, sender, receiver);
        }
    }

    private void onLikeTriggered(Boolean triggered) {
        if (Boolean.TRUE.equals(triggered)) {
            mHeartLayout.addFavor();
        }
    }
}
