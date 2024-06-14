package com.trtc.uikit.livekit.common.uicomponent.gift;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;

import com.tencent.qcloud.tuicore.TUICore;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.uicomponent.gift.model.TUIGiftUser;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.GiftConstants;
import com.trtc.uikit.livekit.common.uicomponent.gift.service.LikePresenter;

import java.util.Collections;

@SuppressLint("ViewConstructor")
public class TUILikeButton extends FrameLayout {
    private final        String        mRoomId;
    private              LikePresenter mPresenter;
    private              long          mSendLikeDate;
    private              int           mCurrentLikeCount;
    private static final int           mMaxLikeCount = 20;
    private static final int           mMinDuration  = 5;

    public TUILikeButton(Context context, String roomId) {
        super(context);
        this.mRoomId = roomId;
        initView(context);
    }

    public void initView(Context context) {
        LayoutInflater.from(context).inflate(R.layout.livekit_gift_like_button, this);
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
                public void onReceiveLikeMessage(TUIGiftUser sender) {
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
