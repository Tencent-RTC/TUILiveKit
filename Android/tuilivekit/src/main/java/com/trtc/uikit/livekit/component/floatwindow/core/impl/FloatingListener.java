package com.trtc.uikit.livekit.component.floatwindow.core.impl;

import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.FrameLayout;

import com.trtc.uikit.livekit.component.floatwindow.core.FloatWindowObserver;
import com.trtc.uikit.livekit.component.floatwindow.core.impl.viewmanager.FloatViewManager;

public class FloatingListener implements View.OnTouchListener {

    private float   mTouchDownPointX;
    private float   mTouchDownPointY;
    private float   mCurPointX;
    private float   mCurPointY;
    private boolean mIsActionDrag;

    private ViewGroup.LayoutParams mOldParams;

    private final FloatViewManager    mFloatViewManager;
    private final FloatWindowConfig   mFloatWindowConfig;
    private final FloatWindowObserver mFloatWindowObserver;

    public FloatingListener(FloatViewManager viewManager, FloatWindowConfig config, FloatWindowObserver observer) {
        mFloatViewManager = viewManager;
        mFloatWindowObserver = observer;
        mFloatWindowConfig = config;
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouch(View v, MotionEvent event) {
        int action = event.getAction();
        switch (action) {
            case MotionEvent.ACTION_DOWN:
                mTouchDownPointX = event.getRawX();
                mTouchDownPointY = event.getRawY();
                mCurPointX = mTouchDownPointX;
                mCurPointY = mTouchDownPointY;

                mOldParams = v.getLayoutParams();
                mIsActionDrag = false;
                break;

            case MotionEvent.ACTION_MOVE:

                int newX = mFloatViewManager.getX() + (int) ((int) event.getRawX() - mCurPointX);
                int newY = mFloatViewManager.getY() + (int) ((int) event.getRawY() - mCurPointY);
                updateLayout(v, newX, newY);
                updateFlagOfDragAction(event.getRawX(), event.getRawY());
                mCurPointX = (int) event.getRawX();
                mCurPointY = (int) event.getRawY();
                break;

            case MotionEvent.ACTION_UP:
                if (mIsActionDrag) {
                    if (mFloatWindowConfig.attachToSideWithAnimation) {
                        int screenWidth = mFloatViewManager.getParentWidth();
                        if (mCurPointX > (screenWidth / 2.F)) {
                            startScroll(v, (int) event.getRawX(), screenWidth - v.getWidth(), false);
                        } else {
                            startScroll(v, (int) event.getRawX(), 0, true);
                        }
                    } else {
                        autoMoveToScreenEdge(v);
                    }
                } else {
                    moveBackToOriginalPosition(v);
                    handleClickAction();
                }
                break;

            default:
                break;
        }
        return true;
    }

    private void updateFlagOfDragAction(float xMovePoint, float yMovePoint) {
        float xDistance = Math.abs(xMovePoint - mTouchDownPointX);
        float yDistance = Math.abs(yMovePoint - mTouchDownPointY);
        if (xDistance >= mFloatWindowConfig.maxMoveDistanceOnClick
                || yDistance >= mFloatWindowConfig.maxMoveDistanceOnClick) {
            mIsActionDrag = true;
        }
    }

    private void handleClickAction() {
        if (mFloatWindowObserver != null) {
            mFloatWindowObserver.onClickWindow();
        }
    }

    private void moveBackToOriginalPosition(View view) {
        if (mOldParams == null) {
            return;
        }
        ViewGroup.LayoutParams layoutParams = view.getLayoutParams();
        layoutParams.width = mOldParams.width;
        layoutParams.height = mOldParams.height;
        if (layoutParams instanceof WindowManager.LayoutParams) {
            WindowManager.LayoutParams pl = (WindowManager.LayoutParams) layoutParams;
            WindowManager.LayoutParams oldPl = (WindowManager.LayoutParams) mOldParams;
            pl.x = oldPl.x;
            pl.y = oldPl.y;
        } else if (layoutParams instanceof FrameLayout.LayoutParams) {
            FrameLayout.LayoutParams pl = (FrameLayout.LayoutParams) layoutParams;
            FrameLayout.LayoutParams oldPl = (FrameLayout.LayoutParams) mOldParams;
            pl.leftMargin = oldPl.leftMargin;
            pl.topMargin = oldPl.topMargin;
        }
        mFloatViewManager.updateViewLayout(view, layoutParams);
    }

    private void autoMoveToScreenEdge(View view) {
        int maxPositionX = getMaxPositionX();
        int newX = mFloatViewManager.getX() > (maxPositionX >> 1) ? maxPositionX : mFloatWindowConfig.marginToEdge;
        mFloatViewManager.setX(newX);
        mFloatViewManager.updateViewLayout(view, mFloatViewManager.getLayoutParams());
        if (mFloatWindowObserver != null) {
            mFloatWindowObserver.onMove(mFloatViewManager.getX(), mFloatViewManager.getY());
        }
    }

    private void updateLayout(View view, int newX, int newY) {
        int x = newX;
        int y = newY;
        x = Math.max(x, mFloatWindowConfig.marginToEdge);
        x = Math.min(x, getMaxPositionX());
        y = Math.max(y, mFloatWindowConfig.marginToEdge);
        y = Math.min(y, getMaxPositionY());
        mFloatViewManager.setX(x);
        mFloatViewManager.setY(y);
        mFloatViewManager.updateViewLayout(view, mFloatViewManager.getLayoutParams());
    }

    private int getMaxPositionX() {
        int floatViewWidth = mFloatWindowConfig.windowWidth;
        return mFloatViewManager.getParentWidth() - floatViewWidth - mFloatWindowConfig.marginToEdge;
    }

    private int getMaxPositionY() {
        int floatViewHeight = mFloatWindowConfig.windowHeight;
        return mFloatViewManager.getParentHeight() - floatViewHeight - mFloatWindowConfig.marginToEdge;
    }

    private void startScroll(View view, int start, int end, boolean isLeft) {
        int screenWidth = mFloatViewManager.getParentWidth();
        ValueAnimator valueAnimator = ValueAnimator.ofFloat(start, end).setDuration(300);
        valueAnimator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
            @Override
            public void onAnimationUpdate(ValueAnimator animation) {
                int width = view.getWidth();
                int x = 0;
                if (isLeft) {
                    x = (int) (start * (1 - animation.getAnimatedFraction()));
                } else {
                    float end = (screenWidth - start - width) * animation.getAnimatedFraction();
                    x = (int) (start + end);
                }
                mFloatViewManager.setX(x);
                if (mFloatWindowObserver != null) {
                    mFloatWindowObserver.onMove(mFloatViewManager.getX(), mFloatViewManager.getY());
                }
                calculateHeight(view);
                mFloatViewManager.updateViewLayout(view, mFloatViewManager.getLayoutParams());
            }
        });
        valueAnimator.start();
    }

    private void calculateHeight(View view) {
        int height = view.getHeight();
        int screenHeight = mFloatViewManager.getParentHeight();
        int resourceId = view.getResources().getIdentifier("status_bar_height", "dimen", "android");
        int statusBarHeight = view.getResources().getDimensionPixelSize(resourceId);
        if (mFloatViewManager.getY() < 0) {
            mFloatViewManager.setY(0);
        } else if (mFloatViewManager.getY() > (screenHeight - height - statusBarHeight)) {
            mFloatViewManager.setY(screenHeight - height - statusBarHeight);
        }
    }
}
