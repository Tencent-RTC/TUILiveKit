package com.trtc.uikit.livekit.common.view;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.view.View;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.R;

import java.util.ArrayList;
import java.util.List;

public class RippleView extends View {
    private static final int STROKE_WIDTH_DEFAULT = 2;
    private static final int SPEED_DEFAULT        = 1;
    private static final int DENSITY_DEFAULT      = 100;
    private static final int VIEW_WIDTH_DEFAULT   = 120;
    private static final int MIN_ALPHA            = 40;

    private Paint mPaint;
    private float mWidth;
    private float mHeight;

    private final List<Circle> mRipples = new ArrayList<>();

    private int   mSpeed   = SPEED_DEFAULT;
    private int   mDensity = DENSITY_DEFAULT;
    private int   mColor;
    private float mStartRadius;

    public RippleView(Context context) {
        this(context, null);
    }

    public RippleView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public RippleView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        TypedArray tya = context.obtainStyledAttributes(attrs, R.styleable.RippleView);
        mColor = tya.getColor(R.styleable.RippleView_color, Color.BLUE);
        mStartRadius = tya.getDimension(R.styleable.RippleView_start_radius, 0f);
        tya.recycle();
        init();
    }

    private void init() {
        mPaint = new Paint();
        mPaint.setColor(mColor);
        mPaint.setStrokeWidth(ScreenUtil.dip2px(STROKE_WIDTH_DEFAULT));
        mPaint.setStyle(Paint.Style.STROKE);
        mPaint.setStrokeCap(Paint.Cap.ROUND);
        mPaint.setAntiAlias(true);

        Circle c = new Circle(mStartRadius, 255);
        mRipples.add(c);
        setBackgroundColor(Color.TRANSPARENT);
    }

    @Override
    public void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        drawCircle(canvas);
    }

    private void drawCircle(Canvas canvas) {
        canvas.save();
        for (int i = 0; i < mRipples.size(); i++) {
            Circle circle = mRipples.get(i);
            mPaint.setAlpha(circle.alpha);
            float strokeWidth = mPaint.getStrokeWidth();
            canvas.drawCircle(mWidth / 2, mHeight / 2, circle.width, mPaint);
            if (circle.width > (mWidth / 2 - strokeWidth / 2)) {
                mRipples.remove(i);
            } else {
                double alpha = MIN_ALPHA + 255 - circle.width * (255 / ((double) (mWidth / 2 + strokeWidth / 2)));
                circle.alpha = (int) alpha;
                circle.width += mSpeed;
            }
        }

        if (!mRipples.isEmpty()) {
            if (mRipples.get(mRipples.size() - 1).width > mDensity) {
                mRipples.add(new Circle(mStartRadius, 255));
            }
        }

        invalidate();
        canvas.restore();
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        int myWidthSpecMode = MeasureSpec.getMode(widthMeasureSpec);
        int myWidthSpecSize = MeasureSpec.getSize(widthMeasureSpec);
        int myHeightSpecMode = MeasureSpec.getMode(heightMeasureSpec);
        int myHeightSpecSize = MeasureSpec.getSize(heightMeasureSpec);
        if (myWidthSpecMode == MeasureSpec.EXACTLY) {
            mWidth = myWidthSpecSize;
        } else {
            mWidth = ScreenUtil.dip2px(VIEW_WIDTH_DEFAULT);
        }

        if (myHeightSpecMode == MeasureSpec.EXACTLY) {
            mHeight = myHeightSpecSize;
        } else {
            mHeight = ScreenUtil.dip2px(VIEW_WIDTH_DEFAULT);
        }
        setMeasuredDimension((int) mWidth, (int) mHeight);
    }


    private static class Circle {
        float width;
        int   alpha;

        public Circle(float width, int alpha) {
            this.width = width;
            this.alpha = alpha;
        }
    }
}