package com.trtc.uikit.livekit.liveroom.view.audience.component;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Handler;
import android.widget.FrameLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.liveroom.core.LiveKitStore;


@SuppressLint("ViewConstructor")
public class AudienceWaitingPassView extends FrameLayout {
    private static final int MIN_DOT_COUNT = 1;
    private static final int MAX_DOT_COUNT = 3;

    private final Context  mContext;
    private       TextView mTextWaitingPass;
    private       int      mDotCount;
    private       Handler  mTimerHandler;
    private       Runnable mTimerRunnable;


    public AudienceWaitingPassView(@NonNull Context context) {
        super(context);
        mContext = context;
        mDotCount = MIN_DOT_COUNT;
        initView();
    }

    private void initView() {
        inflate(mContext, R.layout.livekit_audience_link_mic_waiting_pass, this);
        ImageFilterView icon = findViewById(R.id.link_mic_audience_icon);
        ImageLoader.load(mContext, icon, LiveKitStore.sharedInstance().selfInfo.avatarUrl.get(),
                R.drawable.livekit_ic_avatar);
        mTextWaitingPass = findViewById(R.id.text_waiting_pass);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        startTimerTask();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        stopTimerTask();
    }

    private void stopTimerTask() {
        if (mTimerHandler != null) {
            mTimerHandler.removeCallbacks(mTimerRunnable);
            mTimerHandler = null;
        }
        mTimerRunnable = null;
    }

    private void startTimerTask() {
        mTimerHandler = new Handler();
        mTimerRunnable = () -> {
            mDotCount++;
            if (mDotCount > MAX_DOT_COUNT) {
                mDotCount = 1;
            }
            StringBuilder text = new StringBuilder(mContext.getString(R.string.livekit_waiting_pass));
            for (int i = 0; i < mDotCount; i++) {
                text.append(".");
            }
            mTextWaitingPass.setText(text.toString());
            mTimerHandler.postDelayed(mTimerRunnable, 1000);
        };
        mTimerHandler.post(mTimerRunnable);
    }
}
