package com.trtc.uikit.livekit.voiceroom.view.dashboard;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager;
import com.trtc.uikit.livekit.voiceroom.view.BasicView;

import java.util.Locale;

public class AnchorDashboardView extends BasicView {

    public AnchorDashboardView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorDashboardView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AnchorDashboardView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_dashboard_view, this,
                true);
    }

    @SuppressLint("DefaultLocale")
    @Override
    public void init(@NonNull VoiceRoomManager voiceRoomManager) {
        super.init(voiceRoomManager);
        TextView textDuration = findViewById(R.id.tv_duration);
        textDuration.setText(formatSecondsTo00(
                (int) (System.currentTimeMillis() - mVoiceRoomManager.getRoomState().createTime) / 1000));

        TextView textViewers = findViewById(R.id.tv_viewers);
        textViewers.setText(String.format("%d", mVoiceRoomManager.getRoomState().liveExtraInfo.maxAudienceCount));

        TextView textMessageCount = findViewById(R.id.tv_message);
        textMessageCount.setText(String.format("%d", mVoiceRoomManager.getRoomState().liveExtraInfo.messageCount));

        TextView textGiftIncome = findViewById(R.id.tv_gift_income);
        textGiftIncome.setText(String.format("%d", mVoiceRoomManager.getRoomState().liveExtraInfo.giftIncome));

        TextView textGiftPeopleCount = findViewById(R.id.tv_gift_people);
        textGiftPeopleCount.setText(String.format("%d",
                mVoiceRoomManager.getRoomState().liveExtraInfo.giftPeopleSet.size()));

        TextView textLikeCount = findViewById(R.id.tv_like);
        textLikeCount.setText(String.format("%d", mVoiceRoomManager.getRoomState().liveExtraInfo.likeCount));

        findViewById(R.id.iv_back).setOnClickListener(view -> {
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
        });
    }

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    private String formatSecondsTo00(int timeSeconds) {
        String timeString = "-- --";
        if (timeSeconds > 0) {
            int hour = timeSeconds / 3600;
            int min = timeSeconds % 3600 / 60;
            int sec = timeSeconds % 60;
            timeString = String.format(Locale.getDefault(), "%02d:%02d:%02d", hour, min, sec);
        }
        return timeString;
    }
}
