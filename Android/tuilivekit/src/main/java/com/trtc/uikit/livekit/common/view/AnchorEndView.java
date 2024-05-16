package com.trtc.uikit.livekit.common.view;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.core.LiveController;
import com.trtc.uikit.livekit.common.core.store.LiveStore;
import com.trtc.uikit.livekit.common.utils.DateTimeUtil;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine;

@SuppressLint("ViewConstructor")
public class AnchorEndView extends BasicView {

    public AnchorEndView(@NonNull Context context, LiveController liveController) {
        super(context, liveController);
    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }

    @SuppressLint("DefaultLocale")
    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_end_view, this,
                true);

        TextView textDuration = findViewById(R.id.tv_duration);
        textDuration.setText(DateTimeUtil.formatSecondsTo00(
                (int) (System.currentTimeMillis() - mLiveController.getRoomSate().createTime) / 1000));

        TextView textViewers = findViewById(R.id.tv_viewers);
        textViewers.setText(String.format("%d", mLiveController.getRoomSate().maxAudienceNumber));

        TextView textMessageCount = findViewById(R.id.tv_message);
        textMessageCount.setText(String.format("%d", mLiveController.getRoomSate().messageCount));

        TextView textGiftIncome = findViewById(R.id.tv_gift_income);
        textGiftIncome.setText(String.format("%d", mLiveController.getRoomSate().giftIncome));

        TextView textGiftPeopleCount = findViewById(R.id.tv_gift_people);
        textGiftPeopleCount.setText(String.format("%d", mLiveController.getRoomSate().giftPeopleSet.size()));

        TextView textLikeCount = findViewById(R.id.tv_like);
        textLikeCount.setText(String.format("%d", mLiveController.getRoomSate().likeNumber));

        findViewById(R.id.iv_back).setOnClickListener(view -> {
            LiveKitStore.sharedInstance().selfRoomInfo.userLiveStatus.set(TUILiveDefine.UserLiveStatus.NONE);
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        });
    }
}
