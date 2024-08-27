package com.trtc.uikit.livekit.common.uicomponent.dashboard;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.utils.DateTimeUtil;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;

@SuppressLint("ViewConstructor")
public class AnchorDashboardView extends BasicView {

    public AnchorDashboardView(@NonNull Context context, LiveController liveController) {
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
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_dashboard_view, this,
                true);

        TextView textDuration = findViewById(R.id.tv_duration);
        textDuration.setText(DateTimeUtil.formatSecondsTo00(
                (int) (System.currentTimeMillis() - mLiveController.getRoomState().createTime) / 1000));

        TextView textViewers = findViewById(R.id.tv_viewers);
        textViewers.setText(String.format("%d", mLiveController.getRoomState().liveExtraInfo.maxAudienceCount));

        TextView textMessageCount = findViewById(R.id.tv_message);
        textMessageCount.setText(String.format("%d", mLiveController.getRoomState().liveExtraInfo.messageCount));

        TextView textGiftIncome = findViewById(R.id.tv_gift_income);
        textGiftIncome.setText(String.format("%d", mLiveController.getRoomState().liveExtraInfo.giftIncome));

        TextView textGiftPeopleCount = findViewById(R.id.tv_gift_people);
        textGiftPeopleCount.setText(String.format("%d",
                mLiveController.getRoomState().liveExtraInfo.giftPeopleSet.size()));

        TextView textLikeCount = findViewById(R.id.tv_like);
        textLikeCount.setText(String.format("%d", mLiveController.getRoomState().liveExtraInfo.likeCount));

        findViewById(R.id.iv_back).setOnClickListener(view -> {
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        });
    }
}
