package com.trtc.uikit.component.gift;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;

import com.trtc.uikit.component.gift.store.GiftStore;

@SuppressLint("ViewConstructor")
public class LikeButton extends FrameLayout {
    private              String mRoomId;
    private              long   mSendLikeDate;
    private              int    mCurrentLikeCount;
    private static final int    mMaxLikeCount = 20;
    private static final int    mMinDuration  = 5;

    public LikeButton(Context context) {
        this(context, null);
    }

    public LikeButton(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LikeButton(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        LayoutInflater.from(context).inflate(R.layout.livekit_gift_like_button, this);
        initView();
    }

    public void init(String roomId) {
        mRoomId = roomId;
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
        if (mCurrentLikeCount >= mMaxLikeCount) {
            sendLikeByService();
        }
        int duration = (int) (System.currentTimeMillis() / 1000 - mSendLikeDate);
        if (duration >= mMinDuration) {
            sendLikeByService();
        } else {
            mCurrentLikeCount += 1;
        }
    }

    private void sendLikeByService() {
        GiftStore.sharedInstance().mLikeIMService.sendGroupLikeMessage(mRoomId);
        mCurrentLikeCount = 0;
        mSendLikeDate = System.currentTimeMillis() / 1000;
    }

}
