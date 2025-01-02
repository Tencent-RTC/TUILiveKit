package com.trtc.uikit.livekit.livestream.view.audience.floatwindow;

import android.annotation.SuppressLint;
import android.content.Context;

import androidx.annotation.NonNull;

import com.trtc.uikit.livekit.livestream.view.BasicView;
import com.trtc.uikit.livekit.livestreamcore.LiveCoreView;

@SuppressLint("ViewConstructor")
public class FloatViewAudienceView extends BasicView {

    public FloatViewAudienceView(@NonNull Context context, LiveCoreView coreView) {
        super(context);
        addView(coreView);
    }

    @Override
    protected void initView() {

    }

    @Override
    protected void refreshView() {

    }

    @Override
    protected void addObserver() {

    }

    @Override
    protected void removeObserver() {

    }
}
