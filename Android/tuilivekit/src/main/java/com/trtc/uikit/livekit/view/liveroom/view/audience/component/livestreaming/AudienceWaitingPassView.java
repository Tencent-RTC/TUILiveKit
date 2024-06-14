package com.trtc.uikit.livekit.view.liveroom.view.audience.component.livestreaming;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Handler;
import android.view.LayoutInflater;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.common.view.BasicView;
import com.trtc.uikit.livekit.manager.LiveController;


@SuppressLint("ViewConstructor")
public class AudienceWaitingPassView extends BasicView {
    private static final int MIN_DOT_COUNT = 1;
    private static final int MAX_DOT_COUNT = 3;

    private TextView mTextWaitingPass;
    private int      mDotCount;
    private Handler  mTimerHandler;
    private Runnable mTimerRunnable;

    public AudienceWaitingPassView(@NonNull Context context, LiveController controller) {
        super(context, controller);
        mDotCount = MIN_DOT_COUNT;
    }

    @Override
    protected void initView() {
        LayoutInflater.from(mContext).inflate(R.layout.livekit_audience_link_mic_waiting_pass, this, true);
        bindViewId();

        initUserAvatarView();
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

    @Override
    protected void addObserver() {
    }

    @Override
    protected void removeObserver() {
    }

    private void initUserAvatarView() {
        ImageFilterView icon = findViewById(R.id.link_mic_audience_icon);
        ImageLoader.load(mContext, icon, mLiveController.getUserState().selfInfo.avatarUrl.get(),
                R.drawable.livekit_ic_avatar);
    }

    private void bindViewId() {
        mTextWaitingPass = findViewById(R.id.text_waiting_pass);
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
