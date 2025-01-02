package com.trtc.uikit.livekit.component.floatwindow.core.impl;

import android.app.Activity;
import android.content.Context;
import android.view.View;

import com.trtc.tuikit.common.system.ContextProvider;
import com.trtc.uikit.livekit.component.floatwindow.core.FloatWindow;
import com.trtc.uikit.livekit.component.floatwindow.core.FloatWindowObserver;


public final class FloatWindowImpl extends FloatWindow {

    private static final FloatWindow INSTANCE = new FloatWindowImpl();

    private final FloatWindowCore mFloatWindowCore;

    public static FloatWindow getInstance() {
        return INSTANCE;
    }

    public static FloatWindow getFloatWindowOnActivity(Activity activity) {
        return new FloatWindowImpl(activity);
    }

    private FloatWindowImpl() {
        this(ContextProvider.getApplicationContext());
    }

    private FloatWindowImpl(Context context) {
        mFloatWindowCore = new FloatWindowCore(context);
    }

    @Override
    public boolean isShowing() {
        return mFloatWindowCore.isShowing();
    }

    @Override
    public boolean hasPermission() {
        return mFloatWindowCore.hasPermission();
    }

    @Override
    public void requestPermission() {
        mFloatWindowCore.requestPermission();
    }

    @Override
    public void show() {
        if (!mFloatWindowCore.isShowing()) {
            mFloatWindowCore.show();
        }
    }

    @Override
    public void close() {
        if (mFloatWindowCore.isShowing()) {
            mFloatWindowCore.close();
        }
    }

    @Override
    public void setView(View view) {
        mFloatWindowCore.setContentView(view);
    }

    @Override
    public void setObserver(FloatWindowObserver observer) {
        mFloatWindowCore.setObserver(observer);
    }

    @Override
    public void setSize(int width, int height) {
        FloatWindowConfig config = mFloatWindowCore.getWindowConfig();
        config.windowWidth = width;
        config.windowHeight = height;
        mFloatWindowCore.setSize(width, height);
    }

    @Override
    public void setLocation(int x, int y) {
        mFloatWindowCore.setLocation(x, y);
    }

    @Override
    public void setMarginToEdge(int margin) {
        FloatWindowConfig config = mFloatWindowCore.getWindowConfig();
        config.marginToEdge = margin;
    }

    @Override
    public void setAttachToSideWithAnimation(boolean attachToSideWithAnimation) {
        FloatWindowConfig config = mFloatWindowCore.getWindowConfig();
        config.attachToSideWithAnimation = attachToSideWithAnimation;
    }
}
