package com.trtc.uikit.livekit.livestream.view.audience.playing.coguest;

import android.content.Context;
import android.os.Handler;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.FrameLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.utils.widget.ImageFilterView;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.imageloader.ImageLoader;
import com.trtc.uikit.livekit.R;

public class CoGuestRequestFloatView extends FrameLayout {
    private static final int MIN_DOT_COUNT = 1;
    private static final int MAX_DOT_COUNT = 3;

    private TextView mTextWaitingPass;
    private int      mDotCount;
    private Handler  mTimerHandler;
    private Runnable mTimerRunnable;

    public CoGuestRequestFloatView(@NonNull Context context) {
        this(context, null);
    }

    public CoGuestRequestFloatView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CoGuestRequestFloatView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mDotCount = MIN_DOT_COUNT;
        initView();
    }

    protected void initView() {
        LayoutInflater.from(getContext()).inflate(R.layout.livekit_audience_link_mic_waiting_pass, this, true);
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

    private void initUserAvatarView() {
        ImageFilterView icon = findViewById(R.id.link_mic_audience_icon);
        ImageLoader.load(getContext(), icon, TUILogin.getFaceUrl(),
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
            StringBuilder text = new StringBuilder(getContext().getString(R.string.livekit_waiting_pass));
            for (int i = 0; i < mDotCount; i++) {
                text.append(".");
            }
            mTextWaitingPass.setText(text.toString());
            mTimerHandler.postDelayed(mTimerRunnable, 1000);
        };
        mTimerHandler.post(mTimerRunnable);
    }
}
