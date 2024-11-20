package com.trtc.uikit.livekit.component.floatwindow.core.impl.viewmanager;

import android.view.View;
import android.view.ViewGroup;
import android.view.ViewManager;

public abstract class FloatViewManager implements ViewManager {

    public abstract void setWidth(int width);

    public abstract int getWidth();

    public abstract void setHeight(int height);

    public abstract int getHeight();

    public abstract void setX(int x);

    public abstract int getX();

    public abstract void setY(int y);

    public abstract int getY();

    public abstract int getParentWidth();

    public abstract int getParentHeight();

    public abstract ViewGroup.LayoutParams getLayoutParams();

    protected abstract ViewManager getRealViewManager();

    @Override
    public void updateViewLayout(View view, ViewGroup.LayoutParams params) {
        getRealViewManager().updateViewLayout(view, params);
    }

    @Override
    public void addView(View view, ViewGroup.LayoutParams params) {
        getRealViewManager().addView(view, params);
    }

    @Override
    public void removeView(View view) {
        getRealViewManager().removeView(view);
    }
}
