package com.trtc.uikit.livekit.component.gift;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.component.gift.store.GiftStore;

@SuppressLint("ViewConstructor")
public class LikeButton extends FrameLayout {
    private              String mRoomId;
    private              long   mSendLikeTime;
    private static final int    MIN_SEND_DURATION_MS = 2000;

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
        mRoomId = roomId;
        GiftStore.sharedInstance().init(roomId);
    }

    private void initView() {
        setOnClickListener(v -> sendLike());
    }

    public void setImageResource(int resId) {
        ImageView imageLike = findViewById(R.id.iv_like);
        imageLike.setImageResource(resId);
    }

    private void sendLike() {
        GiftStore.sharedInstance().mLikeIMService.likeByLocal(mRoomId);
        long duration = System.currentTimeMillis() - mSendLikeTime;
        if (duration >= MIN_SEND_DURATION_MS) {
            GiftStore.sharedInstance().mLikeIMService.sendGroupLikeMessage(mRoomId);
            mSendLikeTime = System.currentTimeMillis();
        }
    }
}
