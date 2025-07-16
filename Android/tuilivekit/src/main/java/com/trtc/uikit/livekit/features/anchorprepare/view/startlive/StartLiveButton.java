package com.trtc.uikit.livekit.features.anchorprepare.view.startlive;

import android.content.Context;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.view.Gravity;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.content.res.AppCompatResources;
import androidx.appcompat.widget.AppCompatButton;

import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorprepare.manager.AnchorPrepareManager;

public class StartLiveButton extends AppCompatButton {
    private AnchorPrepareManager mManager;

    public StartLiveButton(@NonNull Context context) {
        this(context, null);
    }

    public StartLiveButton(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public StartLiveButton(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        initView();
    }

    public void init(AnchorPrepareManager manager) {
        mManager = manager;
    }

    private void initView() {
        setBackground(AppCompatResources.getDrawable(getContext(), R.drawable.anchor_prepare_round_button_background));
        setText(R.string.common_start_live);
        setTextSize(TypedValue.COMPLEX_UNIT_SP, 20f);
        setTextColor(getResources().getColor(android.R.color.white));
        setGravity(Gravity.CENTER);
        setAllCaps(false);
        setTypeface(null, Typeface.BOLD);

        setOnClickListener(v -> {
            if (mManager != null) {
                mManager.startLive();
            }
        });
    }
}
