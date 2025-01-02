package com.trtc.uikit.livekit.component.floatwindow.core.impl.viewmanager;

import android.content.Context;
import android.graphics.PixelFormat;
import android.os.Build;
import android.view.Gravity;
import android.view.ViewGroup;
import android.view.ViewManager;
import android.view.WindowManager;

import com.tencent.qcloud.tuicore.util.ScreenUtil;

public final class SystemViewManager extends FloatViewManager {

    private final Context                    mContext;
    private final WindowManager              mWindowManager;
    private final WindowManager.LayoutParams mWindowLayoutParams = new WindowManager.LayoutParams();

    public SystemViewManager(Context context) {
        mContext = context;
        mWindowManager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        initWindowLayoutParams();
    }

    private void initWindowLayoutParams() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            mWindowLayoutParams.type = WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY;
        } else {
            mWindowLayoutParams.type = WindowManager.LayoutParams.TYPE_PHONE;
        }
        mWindowLayoutParams.flags = WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL
                | WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE
                | WindowManager.LayoutParams.FLAG_LAYOUT_NO_LIMITS;

        mWindowLayoutParams.gravity = Gravity.START | Gravity.TOP;
        mWindowLayoutParams.format = PixelFormat.TRANSPARENT;
    }

    @Override
    public void setWidth(int width) {
        mWindowLayoutParams.width = width;
    }

    @Override
    public int getWidth() {
        return mWindowLayoutParams.width;
    }

    @Override
    public void setHeight(int height) {
        mWindowLayoutParams.height = height;
    }

    @Override
    public int getHeight() {
        return mWindowLayoutParams.height;
    }

    @Override
    public void setX(int x) {
        mWindowLayoutParams.x = x;
    }

    @Override
    public int getX() {
        return mWindowLayoutParams.x;
    }

    @Override
    public void setY(int y) {
        mWindowLayoutParams.y = y;
    }

    @Override
    public int getY() {
        return mWindowLayoutParams.y;
    }

    @Override
    public int getParentWidth() {
        return ScreenUtil.getScreenWidth(mContext);
    }

    @Override
    public int getParentHeight() {
        return ScreenUtil.getScreenHeight(mContext);
    }

    @Override
    public ViewGroup.LayoutParams getLayoutParams() {
        return mWindowLayoutParams;
    }

    @Override
    protected ViewManager getRealViewManager() {
        return mWindowManager;
    }
}
