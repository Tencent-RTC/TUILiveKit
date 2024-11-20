package com.trtc.uikit.livekit.component.floatwindow.core;

import android.app.Activity;
import android.view.View;

import com.trtc.uikit.livekit.component.floatwindow.core.impl.FloatWindowImpl;

public abstract class FloatWindow {

    public static FloatWindow getInstance() {
        return FloatWindowImpl.getInstance();
    }

    public static FloatWindow getFloatWindowOnActivity(Activity activity) {
        return FloatWindowImpl.getFloatWindowOnActivity(activity);
    }

    public abstract boolean isShowing();

    public abstract boolean hasPermission();

    public abstract void requestPermission();

    public abstract void show();

    public abstract void close();

    public abstract void setView(View view);

    public abstract void setObserver(FloatWindowObserver observer);

    public abstract void setSize(int width, int height);

    public abstract void setLocation(int x, int y);

    public abstract void setMarginToEdge(int margin);

    public abstract void setAttachToSideWithAnimation(boolean attachToSideWithAnimation);
}
