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
import androidx.lifecycle.Observer;

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

    private final Observer<Integer> mViewersCountObserver = this::onViewersCountChange;

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
        mLiveManager.getDashboardState().maxViewersCount.observeForever(mViewersCountObserver);
    }

    @Override
    protected void removeObserver() {
        mLiveManager.getDashboardState().maxViewersCount.removeObserver(mViewersCountObserver);
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
        mTextDuration.setText(formatSecondsTo00((long) mDashboardState.duration / 1000));
    }

    private void initViewersCountView() {
        updateViewersCountView(mDashboardState.maxViewersCount.getValue());
    }

    @SuppressLint("DefaultLocale")
    private void updateViewersCountView(int count) {
        mTextViewersCount.setText(String.format("%d", count));
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

    private String formatSecondsTo00(long timeSeconds) {
        int second = (int) (timeSeconds % 60);
        long minuteTemp = timeSeconds / 60;
        String secondFormat = second >= 10 ? (second + "") : ("0" + second);
        if (minuteTemp > 0) {
            int minute = (int) (minuteTemp % 60);
            long hour = minuteTemp / 60;
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

    private void onViewersCountChange(int count) {
        updateViewersCountView(count);
    }
}
