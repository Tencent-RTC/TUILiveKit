package com.trtc.uikit.livekit.livestream.view.anchor.dashboard;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.livestream.view.BasicView;

@SuppressLint("ViewConstructor")
public class AnchorDashboardView extends BasicView {

    private TextView  mTextDuration;
    private TextView  mTextViewersCount;
    private TextView  mTextMessageCount;
    private TextView  mTextGiftCount;
    private TextView  mTextGiftIncome;
    private TextView  mTextLikeCount;
    private ImageView mImageBack;

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
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    @SuppressLint("DefaultLocale")
    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_anchor_dashboard_view, this, true);
        mTextDuration = findViewById(R.id.tv_duration);
        mTextViewersCount = findViewById(R.id.tv_viewers);
        mTextMessageCount = findViewById(R.id.tv_message);
        mTextGiftIncome = findViewById(R.id.tv_gift_income);
        mTextGiftCount = findViewById(R.id.tv_gift_people);
        mTextLikeCount = findViewById(R.id.tv_like);
        mImageBack = findViewById(R.id.iv_back);
    }

    @Override
    protected void refreshView() {
        initDurationView();
        initViewersCountView();
        initMessageInfoView();
        initGiftInfoView();
        initLikeInfoView();
        initBackView();
    }

    private void initDurationView() {
        mTextDuration.setText(formatSecondsTo00((int) mDashboardState.duration / 1000));
    }

    @SuppressLint("DefaultLocale")
    private void initViewersCountView() {
        mTextViewersCount.setText(String.format("%d", mDashboardState.maxViewersCount));
    }

    @SuppressLint("DefaultLocale")
    private void initMessageInfoView() {
        mTextMessageCount.setText(String.format("%d", mDashboardState.messageCount));

    }

    @SuppressLint("DefaultLocale")
    private void initGiftInfoView() {
        mTextGiftIncome.setText(String.format("%d", mDashboardState.giftIncome));
        mTextGiftCount.setText(String.format("%d", mDashboardState.giftPeopleSet.size()));
    }

    @SuppressLint("DefaultLocale")
    private void initLikeInfoView() {
        mTextLikeCount.setText(String.format("%d", mDashboardState.likeCount));

    }

    private void initBackView() {
        mImageBack.setOnClickListener(view -> {
            if (mContext instanceof Activity) {
                ((Activity) mContext).finish();
            }
            mDashboardState.reset();
        });
    }

    private String formatSecondsTo00(int timeSeconds) {
        int second = timeSeconds % 60;
        int minuteTemp = timeSeconds / 60;
        String secondFormat = second >= 10 ? (second + "") : ("0" + second);
        if (minuteTemp > 0) {
            int minute = minuteTemp % 60;
            int hour = minuteTemp / 60;
            String s = minute >= 10 ? (minute + "") : ("0" + minute);
            if (hour > 0) {
                return (hour >= 10 ? (hour + "") : ("0" + hour)) + ":" + s + ":" + secondFormat;
            } else {
                return s + ":" + secondFormat;
            }
        } else {
            return "00:" + secondFormat;
        }
    }
}
