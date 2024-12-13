package com.trtc.uikit.livekit.component.floatwindow.core.impl;

import android.app.Activity;
import android.content.Context;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import com.tencent.qcloud.tuicore.permission.PermissionRequester;
import com.trtc.uikit.livekit.component.floatwindow.core.FloatWindowObserver;
import com.trtc.uikit.livekit.component.floatwindow.core.impl.viewmanager.ActivityViewManager;
import com.trtc.uikit.livekit.component.floatwindow.core.impl.viewmanager.FloatViewManager;
import com.trtc.uikit.livekit.component.floatwindow.core.impl.viewmanager.SystemViewManager;

public final class FloatWindowCore {

    private static final String TAG = "FloatWindowCore";

    private final ViewGroup           mRootView;
    private final FloatViewManager    mFloatViewManager;
    private final FloatWindowConfig   mWindowConfig = new FloatWindowConfig();
    private       FloatWindowObserver mFloatWindowObserver;

    public FloatWindowCore(Context context) {
        if (context instanceof Activity) {
            mFloatViewManager = new ActivityViewManager((Activity) context);
        } else {
            mFloatViewManager = new SystemViewManager(context);
        }
        mFloatViewManager.setWidth(mWindowConfig.windowWidth);
        mFloatViewManager.setHeight(mWindowConfig.windowHeight);
        mRootView = new FrameLayout(context);
        initListener();
    }

    private void initListener() {
        View.OnTouchListener listener = new FloatingListener(mFloatViewManager, mWindowConfig,
                new FloatWindowObserver() {
                    @Override
                    public void onClickWindow() {
                        if (mFloatWindowObserver != null) {
                            mFloatWindowObserver.onClickWindow();
                        }
                    }

                    @Override
                    public void onMove(int x, int y) {
                        if (mFloatWindowObserver != null) {
                            mFloatWindowObserver.onMove(x, y);
                        }
                    }
                });
        mRootView.setOnTouchListener(listener);
    }

    public void setContentView(View view) {
        Log.i(TAG, "setContentView:" + view);
        mRootView.removeAllViews();
        if (view != null) {
            mRootView.addView(view);
        }
    }

    public void show() {
        if (!hasPermission()) {
            Log.i(TAG, "show failed: no permission");
            return;
        }
        int x = mFloatViewManager.getParentWidth() - mWindowConfig.marginToEdge - mFloatViewManager.getWidth();
        int y = mWindowConfig.marginToEdge;
        mFloatViewManager.setX(x);
        mFloatViewManager.setY(y);
        mFloatViewManager.addView(mRootView, mFloatViewManager.getLayoutParams());
    }

    public boolean hasPermission() {
        if (mFloatViewManager instanceof SystemViewManager) {
            return PermissionRequester.newInstance(PermissionRequester.FLOAT_PERMISSION).has();
        }
        return true;
    }

    public void requestPermission() {
        if (mFloatViewManager instanceof SystemViewManager) {
            PermissionRequester.newInstance(
                    PermissionRequester.FLOAT_PERMISSION, PermissionRequester.BG_START_PERMISSION).request();
        }
    }

    public void close() {
        mRootView.removeAllViews();
        mFloatViewManager.removeView(mRootView);
    }

    public boolean isShowing() {
        return mRootView.getParent() != null;
    }

    public FloatWindowConfig getWindowConfig() {
        return mWindowConfig;
    }

    public void setLocation(int x, int y) {
        mFloatViewManager.setX(x);
        mFloatViewManager.setY(y);
        updateWindowLayoutParams();
    }

    public void setSize(int w, int h) {
        mFloatViewManager.setWidth(w);
        mFloatViewManager.setHeight(h);
        updateWindowLayoutParams();
    }

    public void setObserver(FloatWindowObserver observer) {
        mFloatWindowObserver = observer;
    }

    private void updateWindowLayoutParams() {
        if (mRootView.getParent() != null) {
            mFloatViewManager.updateViewLayout(mRootView, mFloatViewManager.getLayoutParams());
        }
    }
}
