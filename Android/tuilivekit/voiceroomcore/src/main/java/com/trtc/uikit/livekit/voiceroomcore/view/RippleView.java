package com.trtc.uikit.livekit.voiceroomcore.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.view.View;

import androidx.annotation.NonNull;

import com.tencent.qcloud.tuicore.util.ScreenUtil;
import com.trtc.uikit.livekit.voiceroomcore.R;

public class RippleView extends View {
    private static final int STROKE_WIDTH_DEFAULT = 2;
    private static final int SPEED_DEFAULT        = 1;
    private static final int VIEW_WIDTH_DEFAULT   = 120;
    private static final int MIN_ALPHA            = 40;

    private Paint mPaint;
    private float mWidth;
    private float mHeight;

    private final Circle mCircle;

    private static final int   mSpeed = SPEED_DEFAULT;
    private final        int   mColor;
    private final        float mStartRadius;

    public RippleView(Context context) {
        this(context, null);
    }

    public RippleView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public RippleView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        @SuppressLint("CustomViewStyleable")
        TypedArray tya = context.obtainStyledAttributes(attrs, R.styleable.VoiceRoomCoreRippleView);
        mColor = tya.getColor(R.styleable.VoiceRoomCoreRippleView_color, Color.BLUE);
        mStartRadius = tya.getDimension(R.styleable.VoiceRoomCoreRippleView_start_radius, 0f);
        mCircle = new Circle(mStartRadius, 255);
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
        setBackgroundColor(Color.TRANSPARENT);
    }

    @Override
    public void onDraw(@NonNull Canvas canvas) {
        super.onDraw(canvas);
        drawCircle(canvas);
    }

    private void drawCircle(Canvas canvas) {
        canvas.save();
        mPaint.setAlpha(mCircle.alpha);
        float strokeWidth = mPaint.getStrokeWidth();
        canvas.drawCircle(mWidth / 2, mHeight / 2, mCircle.width, mPaint);
        if (mCircle.width > (mWidth / 2 - strokeWidth / 2)) {
            mCircle.width = mStartRadius;
            mCircle.alpha = 255;
        } else {
            double alpha = MIN_ALPHA + 255 - mCircle.width * (255 / ((double) (mWidth / 2 + strokeWidth / 2)));
            mCircle.alpha = (int) alpha;
            mCircle.width += mSpeed;
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