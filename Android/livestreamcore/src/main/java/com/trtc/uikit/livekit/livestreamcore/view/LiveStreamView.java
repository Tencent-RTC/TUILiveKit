package com.trtc.uikit.livekit.livestreamcore.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.cloud.tuikit.engine.common.TUIVideoView;

public class LiveStreamView extends FrameLayout {

    private final TUIVideoView mTUIVideoView;

    public LiveStreamView(@NonNull Context context) {
        this(context, null);
    }

    public LiveStreamView(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LiveStreamView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        mTUIVideoView = new TUIVideoView(context);
        FrameLayout.LayoutParams params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT);
        addView(mTUIVideoView, params);
    }

    public TUIVideoView getTUIVideoView() {
        return mTUIVideoView;
    }
}
