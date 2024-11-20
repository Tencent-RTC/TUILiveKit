package com.trtc.uikit.livekit.component.floatwindow.view;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Path;
import android.graphics.RectF;
import android.graphics.Region;
import android.util.AttributeSet;
import android.widget.FrameLayout;

import androidx.annotation.Nullable;

import com.tencent.qcloud.tuicore.util.ScreenUtil;

public final class RoundFrameLayout extends FrameLayout {

    private int   mRadius;
    private RectF mRect;
    private Path  mPath;

    public RoundFrameLayout(Context context) {
        this(context, null);
    }

    public RoundFrameLayout(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        setWillNotDraw(false);
        mRadius = ScreenUtil.dip2px(12);
        mPath = new Path();
    }

    public void setRadius(int radius) {
        mRadius = radius;
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);
        mRect = new RectF(0, 0, getWidth(), getHeight());
    }

    @Override
    public void draw(Canvas canvas) {
        mPath.reset();
        mPath.addRoundRect(mRect, mRadius, mRadius, Path.Direction.CW);
        int saveCount = canvas.save();
        canvas.clipPath(mPath, Region.Op.INTERSECT);
        super.draw(canvas);
        canvas.restoreToCount(saveCount);
    }
}

