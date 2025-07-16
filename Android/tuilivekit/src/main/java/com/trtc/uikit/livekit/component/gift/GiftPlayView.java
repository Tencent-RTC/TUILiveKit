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

import com.tencent.cloud.tuikit.engine.extension.TUILiveGiftManager.GiftInfo;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.UserInfo;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.service.GiftService;
import com.trtc.uikit.livekit.component.gift.store.TUIGiftStore;
import com.trtc.uikit.livekit.component.gift.store.model.TUILikeData;
import com.trtc.uikit.livekit.component.gift.view.animation.AnimationView;
import com.trtc.uikit.livekit.component.gift.view.animation.ImageAnimationView;
import com.trtc.uikit.livekit.component.gift.view.animation.manager.AnimationPlayer;
import com.trtc.uikit.livekit.component.gift.view.animation.manager.GiftAnimationManager;
import com.trtc.uikit.livekit.component.gift.view.animation.manager.GiftAnimationModel;
import com.trtc.uikit.livekit.component.gift.view.like.GiftHeartLayout;

import java.util.Map;

@SuppressLint("ViewConstructor")
public class GiftPlayView extends FrameLayout implements GiftService.GiftServiceDelegate {

    private static final String TAG                        = "GiftPlayView";
    private static final int    LIKE_ANIMATION_INTERVAL_MS = 100;
    private static final int    LIKE_ANIMATION_COUNT_MAX   = 30;

    private final Context                            mContext;
    private       String                             mRoomId;
    private       ImageAnimationView                 mImageAnimationView;
    private       AnimationView                      mAnimationView;
    private       GiftHeartLayout                    mHeartLayout;
    private       TUIGiftPlayViewListener            mGiftPlayViewListener;
    private final Observer<Map<String, TUILikeData>> mLikeObserver = this::onLikeDataMapChange;
    private final GiftService                        mGiftService  = new GiftService();

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
        LayoutInflater.from(getContext()).inflate(R.layout.gift_layout_animator, this, true);
        initView();
        initAnimationPlayer();
        initImageAnimationPlayer();
    }

    public void init(String roomId) {
        this.mRoomId = roomId;
        this.mAnimationView.setRoomId(mRoomId);
        mGiftService.init(roomId, this);
    }

    @Override
    public void onReceiveGiftMessage(GiftInfo giftInfo, int giftCount, UserInfo sender) {
        playGift(giftInfo, giftCount, sender);
    }

    @Override
    public void onReceiveLikesMessage(int totalLikesDiff, UserInfo sender) {
        playLikeFromOther(totalLikesDiff);
    }

    private void initView() {
        mImageAnimationView = findViewById(R.id.gift_image_anim_view);
        mAnimationView = findViewById(R.id.gift_anim_view);
        mHeartLayout = findViewById(R.id.heart_layout);
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
                    mAnimationView.playAnimation(model.gift.resourceUrl);
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
                    info.giftImageUrl = model.gift.iconUrl;
                    info.giftName = model.gift.name;
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
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        TUIGiftStore.sharedInstance().mLikeDataMap.observeForever(mLikeObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        mGiftService.unInit();
        TUIGiftStore.sharedInstance().mLikeDataMap.removeObserver(mLikeObserver);
        mGiftAnimationManager.stopPlay();
        mGiftImageAnimationManager.stopPlay();
        super.onDetachedFromWindow();
    }

    public void playGiftAnimation(String playUrl) {
        GiftAnimationModel model = new GiftAnimationModel();
        model.gift = new GiftInfo();
        model.gift.resourceUrl = playUrl;
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
        void onReceiveGift(GiftPlayView view, GiftInfo gift, int giftCount, UserInfo sender);

        void onPlayGiftAnimation(GiftPlayView view, GiftInfo gift);
    }

    private void playGift(final GiftInfo gift, int giftCount, UserInfo sender) {
        if (gift == null || sender == null) {
            Log.i(TAG, "playGift failed, gift or sender is null");
            return;
        }
        GiftAnimationModel giftModel = new GiftAnimationModel();
        giftModel.gift = gift;
        giftModel.giftCount = giftCount;
        giftModel.sender = sender;
        giftModel.isFromSelf = TextUtils.equals(sender.userId, TUILogin.getUserId());
        if (TextUtils.isEmpty(gift.resourceUrl)) {
            mGiftImageAnimationManager.add(giftModel);
        } else {
            mGiftAnimationManager.add(giftModel);
        }
        if (mGiftPlayViewListener != null) {
            mGiftPlayViewListener.onReceiveGift(this, gift, giftCount, sender);
        }
    }

    private void playLike() {
        mHeartLayout.addFavor();
    }

    private void playLikeFromSelf() {
        playLike();
    }

    private void playLikeFromOther(int count) {
        int newCount = Math.min(count, LIKE_ANIMATION_COUNT_MAX);
        newCount = Math.max(newCount, 0);
        final int[] likeCount = {newCount};
        Runnable task = new Runnable() {
            @Override
            public void run() {
                if (likeCount[0] > 0) {
                    playLike();
                    postDelayed(this, LIKE_ANIMATION_INTERVAL_MS);
                    likeCount[0]--;
                }
            }
        };
        post(task);
    }

    private void onLikeDataMapChange(Map<String, TUILikeData> likeDataMap) {
        if (TextUtils.isEmpty(mRoomId) || likeDataMap == null) {
            return;
        }
        TUILikeData likeData = likeDataMap.get(mRoomId);
        if (likeData == null || likeData.sender == null) {
            return;
        }
        if (TextUtils.equals(likeData.sender.userId, TUILogin.getUserId())) {
            playLikeFromSelf();
        }
    }
}
