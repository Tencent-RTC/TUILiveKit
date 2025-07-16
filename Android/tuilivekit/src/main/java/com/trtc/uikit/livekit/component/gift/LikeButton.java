package com.trtc.uikit.livekit.component.gift;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.ErrorLocalized;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.gift.service.GiftService;
import com.trtc.uikit.livekit.component.gift.store.TUIGiftStore;
import com.trtc.uikit.livekit.component.gift.store.model.TUILikeData;

import java.util.Map;

@SuppressLint("ViewConstructor")
public class LikeButton extends FrameLayout {
    private static final LiveKitLogger LOGGER                = LiveKitLogger.getComponentLogger("LikeButton");
    private static final String        DEFAULT_AVATAR        = "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar1.png";
    private static final int           LIKE_SEND_INTERVAL_MS = 6 * 1000;

    private int     mLikePendingCount   = 0;
    private long    mLikeLastSendTimeMS = 0;
    private boolean mHasPostLikeTask    = false;

    private final GiftService mGiftService  = new GiftService();
    private final Handler     mMainHandler  = new Handler(Looper.getMainLooper());
    private final Runnable    mSendLikeTask = () -> {
        LOGGER.info("mSendLikeTask run");
        mHasPostLikeTask = false;
        sendLike(mLikePendingCount, System.currentTimeMillis());
    };

    public LikeButton(Context context) {
        this(context, null);
    }

    public LikeButton(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LikeButton(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        LayoutInflater.from(context).inflate(R.layout.gift_layout_like_button, this);
        initView();
    }

    public void init(String roomId) {
        mGiftService.init(roomId, null);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mGiftService.unInit();
    }

    private void initView() {
        setOnClickListener(v -> onLikeClick());
    }

    public void setImageResource(int resId) {
        ImageView imageLike = findViewById(R.id.iv_like);
        imageLike.setImageResource(resId);
    }

    public void onLikeClick() {
        String roomId = mGiftService.getRoomId();
        if (TextUtils.isEmpty(roomId)) {
            LOGGER.warn("roomId is null");
            return;
        }
        Long localLikeCount = TUIGiftStore.sharedInstance().mLocalLikeCountMap.get(roomId);
        localLikeCount = localLikeCount == null ? 1 : localLikeCount + 1;
        TUIGiftStore.sharedInstance().mLocalLikeCountMap.put(roomId, localLikeCount);
        mLikePendingCount++;
        long now = System.currentTimeMillis();
        long elapsed = now - mLikeLastSendTimeMS;
        TUIRoomDefine.UserInfo sender = createSelfUserInfo();
        Map<String, TUILikeData> map = TUIGiftStore.sharedInstance().mLikeDataMap.getValue();
        if (map != null) {
            map.put(roomId, new TUILikeData(sender));
            TUIGiftStore.sharedInstance().mLikeDataMap.setValue(map);
        }
        if (mLikeLastSendTimeMS == 0 || elapsed > LIKE_SEND_INTERVAL_MS) {
            mMainHandler.removeCallbacks(mSendLikeTask);
            mHasPostLikeTask = false;
            sendLike(mLikePendingCount, now);
        } else {
            if (!mHasPostLikeTask) {
                mMainHandler.postDelayed(mSendLikeTask, LIKE_SEND_INTERVAL_MS);
                mHasPostLikeTask = true;
            }
        }
    }

    private void sendLike(int count, long now) {
        mLikePendingCount = 0;
        mLikeLastSendTimeMS = now;
        mGiftService.sendLike(count, new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {

            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                ErrorLocalized.onError(error);
                mLikePendingCount += count;
            }
        });
    }

    private TUIRoomDefine.UserInfo createSelfUserInfo() {
        TUIRoomDefine.UserInfo userInfo = new TUIRoomDefine.UserInfo();
        userInfo.userId = TextUtils.isEmpty(TUILogin.getUserId()) ? "" : TUILogin.getUserId();
        userInfo.userName = TextUtils.isEmpty(TUILogin.getNickName()) ? "" : TUILogin.getNickName();
        userInfo.avatarUrl = TextUtils.isEmpty(TUILogin.getFaceUrl()) ? DEFAULT_AVATAR : TUILogin.getFaceUrl();
        return userInfo;
    }
}
