package com.trtc.uikit.livekit.livestream.view.anchor.pushing.battle;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.RectF;
import android.util.AttributeSet;
import android.view.View;

import androidx.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

public class BattleCountdownBackView extends View {

    private final int   mViewPadding = 80;
    private final Paint mCirclePaint = new Paint();
    private final Paint mArcPaint    = new Paint();
    private final Paint mRipplePaint = new Paint();

    private final RectF mArcRect = new RectF();
    private final Path  mArcPath = new Path();

    private float mCircleRadius  = 0F;
    private int   mRotationAngle = 0;

    private final List<RippleCircle> mRippleCircles = new ArrayList<>();

    public BattleCountdownBackView(Context context) {
        this(context, null);
    }

    public BattleCountdownBackView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        mCirclePaint.setColor(Color.WHITE);
        mCirclePaint.setAlpha(0xCC);
        mCirclePaint.setAntiAlias(true);
        mArcPaint.setColor(Color.WHITE);
        mArcPaint.setStyle(Paint.Style.STROKE);
        mArcPaint.setStrokeWidth(6);
        mArcPaint.setAntiAlias(true);
        mRipplePaint.setColor(Color.WHITE);
        mRipplePaint.setStyle(Paint.Style.STROKE);
        mRipplePaint.setStrokeWidth(2);
        mRipplePaint.setAntiAlias(true);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        int width = getWidth();
        int height = getHeight();
        int size = Math.min(width, height);
        if (mCircleRadius == 0F) {
            mCircleRadius = (size - mViewPadding * 2) / 2F;
            mArcRect.left = (width - mCircleRadius * 2 - mArcPaint.getStrokeWidth() / 2F) / 2F;
            mArcRect.top = (height - mCircleRadius * 2 - mArcPaint.getStrokeWidth() / 2F) / 2F;
            mArcRect.right = mArcRect.left + mCircleRadius * 2;
            mArcRect.bottom = mArcRect.top + mCircleRadius * 2;
        }
        drawCircle(canvas);
        draw2Arc(canvas);
        drawRipple(canvas);
        invalidate();
    }

    private void drawCircle(Canvas canvas) {
        int width = getWidth();
        int height = getHeight();
        canvas.drawCircle(width / 2F, height / 2F, mCircleRadius, mCirclePaint);
    }

    private void draw2Arc(Canvas canvas) {
        mRotationAngle++;
        mRotationAngle = mRotationAngle % 360;
        mArcPath.reset();
        mArcPath.addArc(mArcRect, 120 + mRotationAngle, 60);
        mArcPath.addArc(mArcRect, -120 + mRotationAngle, 180);
        canvas.drawPath(mArcPath, mArcPaint);
    }

    private void drawRipple(Canvas canvas) {
        int width = getWidth();
        int height = getHeight();
        if (mRippleCircles.isEmpty()) {
            // Init with two circles
            float radius = mCircleRadius;
            int alpha = genRippleCircleAlpha(radius);
            mRippleCircles.add(new RippleCircle(radius, alpha));
            radius = mCircleRadius + mViewPadding / 2F;
            alpha = genRippleCircleAlpha(radius);
            mRippleCircles.add(new RippleCircle(radius, alpha));
        }
        for (int i = 0; i < mRippleCircles.size(); i++) {
            RippleCircle circle = mRippleCircles.get(i);
            mRipplePaint.setAlpha(circle.alpha);
            canvas.drawCircle(width / 2F, height / 2F, circle.radius, mRipplePaint);

            if (circle.radius >= (width) / 2F) {
                circle.radius = mCircleRadius;
            } else {
                circle.radius += 1;
            }
            circle.alpha = genRippleCircleAlpha(circle.radius);
        }
    }

    private int genRippleCircleAlpha(float radius) {
        return (int) ((1 - (radius - mCircleRadius) / mViewPadding) * 0xFF);
    }

    private static final class RippleCircle {
        public float radius;
        public int   alpha;

        public RippleCircle(float radius, int alpha) {
            this.radius = radius;
            this.alpha = alpha;
        }
    }
}
