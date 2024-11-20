package com.trtc.uikit.livekit.livestream.view.audience.playing.coguest;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.TextView;

import com.trtc.uikit.livekit.R;

import java.util.Timer;
import java.util.TimerTask;

public class CoGuestPlaceHolderView extends FrameLayout {

    private static final int MAX_DOT_COUNT = 3;
    private static final int MIN_DOT_COUNT = 1;

    private final Context   mContext;
    private       TextView  mTextWaiting;
    private       Timer     mTimer;
    private       TimerTask mTimerTask;
    private       int       mDotCount = MIN_DOT_COUNT;

    public CoGuestPlaceHolderView(Context context) {
        super(context);
        mContext = context;
        init();
    }

    private void init() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_co_guest_place_holder_view, this, true);
        mTextWaiting = findViewById(R.id.tv_waiting);

        mTimer = new Timer();
        mTimerTask = new TimerTask() {
            @SuppressLint("SetTextI18n")
            @Override
            public void run() {
                mTextWaiting.post(() -> {
                    StringBuilder sb = new StringBuilder();
                    for (int i = 0; i < mDotCount; i++) {
                        sb.append(".");
                    }
                    mTextWaiting.setText(mContext.getString(R.string.livekit_waiting_link) + sb);
                    mDotCount++;
                    if (mDotCount > MAX_DOT_COUNT) {
                        mDotCount = MIN_DOT_COUNT;
                    }
                });
            }
        };
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mTimer.schedule(mTimerTask, 0, 1000);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mTimer.cancel();
        mTimerTask.cancel();
    }
}
