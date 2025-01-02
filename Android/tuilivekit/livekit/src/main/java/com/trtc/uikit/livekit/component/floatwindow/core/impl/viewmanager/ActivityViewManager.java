package com.trtc.uikit.livekit.component.floatwindow.core.impl.viewmanager;

import android.app.Activity;
import android.view.Gravity;
import android.view.ViewGroup;
import android.view.ViewManager;
import android.widget.FrameLayout;

import com.tencent.qcloud.tuicore.util.ScreenUtil;

public final class ActivityViewManager extends FloatViewManager {

    private final FrameLayout              mDecorView;
    private final FrameLayout.LayoutParams mLayoutParams;

    public ActivityViewManager(Activity activity) {
        mDecorView = (FrameLayout) activity.getWindow().getDecorView();
        mLayoutParams = new FrameLayout.LayoutParams(0, 0);
        initWindowLayoutParams();
    }

    private void initWindowLayoutParams() {
        mLayoutParams.gravity = Gravity.START | Gravity.TOP;
    }

    @Override
    public void setWidth(int width) {
        mLayoutParams.width = width;
    }

    @Override
    public int getWidth() {
        return mLayoutParams.width;
    }

    @Override
    public void setHeight(int height) {
        mLayoutParams.height = height;
    }

    @Override
    public int getHeight() {
        return mLayoutParams.height;
    }

    @Override
    public void setX(int x) {
        mLayoutParams.leftMargin = x;
    }

    @Override
    public int getX() {
        return mLayoutParams.leftMargin;
    }

    @Override
    public void setY(int y) {
        mLayoutParams.topMargin = y;
    }

    @Override
    public int getY() {
        return mLayoutParams.topMargin;
    }

    @Override
    public int getParentWidth() {
        int w = mDecorView.getWidth();
        if (w == 0) {
            return ScreenUtil.getScreenWidth(mDecorView.getContext());
        }
        return w;
    }

    @Override
    public int getParentHeight() {
        int h = mDecorView.getHeight();
        if (h == 0) {
            return ScreenUtil.getScreenHeight(mDecorView.getContext());
        }
        return h;
    }

    @Override
    public ViewGroup.LayoutParams getLayoutParams() {
        return mLayoutParams;
    }

    @Override
    protected ViewManager getRealViewManager() {
        return mDecorView;
    }
}
