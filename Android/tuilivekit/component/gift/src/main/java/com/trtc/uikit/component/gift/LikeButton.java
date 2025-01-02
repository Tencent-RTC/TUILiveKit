package com.trtc.uikit.component.gift;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.ImageView;

import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.component.gift.store.model.GiftUser;
import com.trtc.uikit.component.gift.service.GiftConstants;
import com.trtc.uikit.component.gift.service.LikePresenter;

import java.util.Collections;

@SuppressLint("ViewConstructor")
public class LikeButton extends FrameLayout {
    private final        Context       mContext;
    private String        mRoomId;
    private LikePresenter mPresenter;
    private long          mSendLikeDate;
    private              int           mCurrentLikeCount;
    private static final int           mMaxLikeCount = 20;
    private static final int           mMinDuration  = 5;

    public LikeButton(Context context) {
        super(context);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_gift_like_button, this);
    }

    public LikeButton(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_gift_like_button, this);
    }

    public LikeButton(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        LayoutInflater.from(mContext).inflate(R.layout.livekit_gift_like_button, this);
    }

    public void init(String roomId) {
        mRoomId = roomId;
        initView();
    }

    public void initView() {
        setOnClickListener(v -> sendLike());
    }

    public void setImageResource(int resId) {
        ImageView imageLike = findViewById(R.id.iv_like);
        imageLike.setImageResource(resId);
    }

    private void sendLike() {
        if (mCurrentLikeCount >= mMaxLikeCount) {
            sendLikeByPresenter();
            mCurrentLikeCount = 0;
            mSendLikeDate = System.currentTimeMillis() / 1000;
        }
        int duration = (int) (System.currentTimeMillis() / 1000 - mSendLikeDate);
        if (duration >= mMinDuration) {
            sendLikeByPresenter();
            mCurrentLikeCount = 0;
            mSendLikeDate = System.currentTimeMillis() / 1000;
        } else {
            mCurrentLikeCount += 1;
            notifyLike();
        }
    }

    private void sendLikeByPresenter() {
        if (mPresenter == null) {
            mPresenter = new LikePresenter(mRoomId) {
                @Override
                public void onReceiveLikeMessage(GiftUser sender) {
                    notifyLike();
                }
            };
            mCurrentLikeCount = 0;
            mSendLikeDate = System.currentTimeMillis() / 1000;
        }
        mPresenter.sendGroupLikeMessage((code, msg) -> {
            if (code == 0) {
                notifyLike();
            }
        });
    }

    private void notifyLike() {
        TUICore.notifyEvent(GiftConstants.KEY_LIKE, GiftConstants.SUB_KEY_LIKE_SEND, Collections.EMPTY_MAP);
    }

    @Override
    protected void onDetachedFromWindow() {
        if (mPresenter != null) {
            mPresenter.destroyPresenter();
        }
        super.onDetachedFromWindow();
    }
}
