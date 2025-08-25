package com.trtc.uikit.livekit.features.endstatistics;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.endstatistics.manager.EndStatisticsManager;
import com.trtc.uikit.livekit.features.endstatistics.state.EndStatisticsState;

import java.util.Locale;

public class AnchorEndStatisticsView extends FrameLayout {
    private final LiveKitLogger LOGGER = LiveKitLogger.getFeaturesLogger("AnchorEndStatisticsView");

    private final EndStatisticsManager mManager = new EndStatisticsManager();
    private final EndStatisticsState   mState   = mManager.getState();

    private final Observer<Long> mLiveDurationObserver    = this::onLiveDurationChange;
    private final Observer<Long> mMaxViewersCountObserver = this::onMaxViewersCountChange;
    private final Observer<Long> mMessageCountObserver    = this::onMessageCountChange;
    private final Observer<Long> mLikeCountObserver       = this::onLikeCountChange;
    private final Observer<Long> mGiftIncomeObserver      = this::onGiftIncomeChange;
    private final Observer<Long> mGiftSenderCountObserver = this::onGiftSenderCountChange;

    private TextView mTextDuration;
    private TextView mTextViewersCount;
    private TextView mTextMessageCount;
    private TextView mTextGiftSenderCount;
    private TextView mTextGiftIncome;
    private TextView mTextLikeCount;

    private EndStatisticsDefine.AnchorEndStatisticsViewListener mListener;

    public AnchorEndStatisticsView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorEndStatisticsView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        initView();
    }

    private void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_anchor_dashboard_view, this, true);
        mTextDuration = findViewById(R.id.tv_duration);
        mTextViewersCount = findViewById(R.id.tv_viewers);
        mTextMessageCount = findViewById(R.id.tv_message);
        mTextGiftIncome = findViewById(R.id.tv_gift_income);
        mTextGiftSenderCount = findViewById(R.id.tv_gift_people);
        mTextLikeCount = findViewById(R.id.tv_like);
        findViewById(R.id.iv_back).setOnClickListener(v -> onExitClick());
    }

    public void init(EndStatisticsDefine.AnchorEndStatisticsInfo info) {
        if (info == null) {
            LOGGER.error("init, info is null");
        } else {
            mManager.setRoomId(info.roomId);
            mManager.setLiveDuration(info.liveDurationMS);
            mManager.setMaxViewersCount(Math.max(0, info.maxViewersCount - 1));
            mManager.setMessageCount(Math.max(0, info.messageCount - 1));
            mManager.setLikeCount(info.likeCount);
            mManager.setGiftIncome(info.giftIncome);
            mManager.setGiftSenderCount(info.giftSenderCount);
            LOGGER.info("init, " + mState.toString());
        }
    }

    public void setListener(EndStatisticsDefine.AnchorEndStatisticsViewListener listener) {
        mListener = listener;
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        addObserver();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeObserver();
    }

    private void addObserver() {
        mState.liveDurationMS.observeForever(mLiveDurationObserver);
        mState.maxViewersCount.observeForever(mMaxViewersCountObserver);
        mState.messageCount.observeForever(mMessageCountObserver);
        mState.likeCount.observeForever(mLikeCountObserver);
        mState.giftIncome.observeForever(mGiftIncomeObserver);
        mState.giftSenderCount.observeForever(mGiftSenderCountObserver);
    }

    private void removeObserver() {
        mState.liveDurationMS.removeObserver(mLiveDurationObserver);
        mState.maxViewersCount.removeObserver(mMaxViewersCountObserver);
        mState.messageCount.removeObserver(mMessageCountObserver);
        mState.likeCount.removeObserver(mLikeCountObserver);
        mState.giftIncome.removeObserver(mGiftIncomeObserver);
        mState.giftSenderCount.removeObserver(mGiftSenderCountObserver);
    }

    private void onExitClick() {
        if (mListener != null) {
            mListener.onCloseButtonClick();
        }
    }

    private void onLiveDurationChange(Long durationMS) {
        int duration = (int) (durationMS / 1000);
        String formatSeconds = mManager.formatSeconds(duration);
        mTextDuration.setText(formatSeconds);
    }

    private void onMaxViewersCountChange(Long count) {
        String info = String.format(Locale.getDefault(), "%d", count);
        mTextViewersCount.setText(info);
    }

    private void onMessageCountChange(Long count) {
        String info = String.format(Locale.getDefault(), "%d", count);
        mTextMessageCount.setText(info);
    }

    private void onLikeCountChange(Long count) {
        String info = String.format(Locale.getDefault(), "%d", count);
        mTextLikeCount.setText(info);
    }

    private void onGiftIncomeChange(Long count) {
        String info = String.format(Locale.getDefault(), "%d", count);
        mTextGiftIncome.setText(info);
    }

    private void onGiftSenderCountChange(Long count) {
        String info = String.format(Locale.getDefault(), "%d", count);
        mTextGiftSenderCount.setText(info);
    }
}
