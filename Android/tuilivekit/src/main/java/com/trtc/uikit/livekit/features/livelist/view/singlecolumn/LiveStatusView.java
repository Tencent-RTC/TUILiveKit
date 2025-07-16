package com.trtc.uikit.livekit.features.livelist.view.singlecolumn;

import android.animation.ValueAnimator;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.view.View;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;

public class LiveStatusView extends View {
    private static final int BAR_COUNT     = 3;
    private static final int BAR_WIDTH     = 2;
    private static final int BAR_HEIGHT    = 7;
    private static final int BAR_SPACING   = 3;
    private static final int ANIM_DURATION = 1000;
    private static final int COLOR         = Color.parseColor("#FFFFFF");

    private       Paint         mBarPaint;
    private final float[]       mBarHeights = new float[3];
    private       ValueAnimator mAnimator;

    public LiveStatusView(Context context) {
        super(context);
        init();
    }

    public LiveStatusView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        mBarPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        mBarPaint.setColor(COLOR);
        mBarPaint.setStyle(Paint.Style.FILL);

        for (int i = 0; i < BAR_COUNT; i++) {
            mBarHeights[i] = dp2px(BAR_HEIGHT);
        }
    }

    @Override
    protected void onDraw(@NonNull Canvas canvas) {
        super.onDraw(canvas);

        int totalWidth = dp2px(BAR_WIDTH * BAR_COUNT + BAR_SPACING * (BAR_COUNT - 1));
        int startX = (getWidth() - totalWidth) / 2;
        int centerY = getHeight() / 2;

        for (int i = 0; i < BAR_COUNT; i++) {
            float left = startX + i * (dp2px(BAR_WIDTH + BAR_SPACING));
            float top = centerY - mBarHeights[i] / 2;
            float right = left + dp2px(BAR_WIDTH);
            float bottom = centerY + mBarHeights[i] / 2;

            canvas.drawRoundRect(left, top, right, bottom, dp2px(2), dp2px(2), mBarPaint);
        }
    }

    private void startAnimation() {
        if (mAnimator != null && mAnimator.isRunning()) {
            mAnimator.cancel();
        }

        mAnimator = ValueAnimator.ofFloat(0, 1);
        mAnimator.setDuration(ANIM_DURATION);
        mAnimator.setRepeatCount(ValueAnimator.INFINITE);
        mAnimator.setRepeatMode(ValueAnimator.REVERSE);
        mAnimator.setInterpolator(new LinearInterpolator());

        mAnimator.addUpdateListener(animation -> {
            float progress = (float) animation.getAnimatedValue();
            for (int i = 0; i < BAR_COUNT; i++) {
                float phase = i * 0.6f;
                float adjustedProgress = (progress + phase) % 1.0f;
                mBarHeights[i] = dp2px(BAR_HEIGHT + (float) (Math.sin(adjustedProgress * Math.PI) * 15));
            }
            invalidate();
        });

        mAnimator.start();
    }

    public void stopAnimation() {
        if (mAnimator != null) {
            mAnimator.cancel();
        }
    }

    private int dp2px(float dp) {
        return (int) (dp * getResources().getDisplayMetrics().density + 0.5f);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        startAnimation();
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        stopAnimation();
    }
}