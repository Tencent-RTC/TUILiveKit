package com.tencent.effect.beautykit.view.widget.indicatorseekbar;

import android.animation.ValueAnimator;
import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.RectF;
import android.graphics.Typeface;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.StateListDrawable;
import android.os.Build;
import android.os.Bundle;
import android.os.Parcelable;

import android.text.TextPaint;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewParent;
import android.view.WindowManager;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;


import androidx.annotation.ColorInt;
import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;

import com.tencent.effect.beautykit.R;
import com.tencent.effect.beautykit.utils.ScreenUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;



public class IndicatorSeekBar extends View {
    private static final int THUMB_MAX_WIDTH = 30;
    private static final String FORMAT_PROGRESS = "${PROGRESS}";
    private static final String FORMAT_TICK_TEXT = "${TICK_TEXT}";
    private Context mContext;
    private Paint mStockPaint;//the paint for seek bar drawing
    private TextPaint mTextPaint;//the paint for mTickTextsArr drawing
    private OnSeekChangeListener mSeekChangeListener;
    private Rect mRect;
    private float mCustomDrawableMaxHeight;//the max height for custom drawable
    private float lastProgress;
    private float mFaultTolerance = -1;//the tolerance for user seek bar touching
    private float mScreenWidth = -1;
    private boolean mClearPadding;
    private SeekParams mSeekParams;//save the params when seeking change.
    //seek bar
    private int mPaddingLeft;
    private int mPaddingRight;
    private int mMeasuredWidth;
    private int mPaddingTop;
    private float mSeekLength;//the total length of seek bar
    private float mSeekBlockLength;//the length for each section part to seek
    private boolean mIsTouching;//user is touching the seek bar
    private float mMax;
    private float mMin;
    private float mProgress;
    private boolean mIsFloatProgress;// true for the progress value in float,otherwise in int.
    private int mScale = 1;//the scale of the float progress.
    private boolean mUserSeekable;
    //true if the user can seek to change the progress,otherwise only can be changed by setProgress().
    private boolean mOnlyThumbDraggable;//only drag the seek bar's thumb can be change the progress
    private boolean mSeekSmoothly;//seek continuously
    private float[] mProgressArr;//save the progress which at tickMark position.
    private boolean mR2L;//right to left,compat local problem.
    //tick texts
    private boolean mShowTickText;//the palace where the tick text show .
    private boolean mShowBothTickTextsOnly;//show the tick texts on the both ends of seek bar before.
    private int mTickTextsHeight;//the height of text
    private String[] mTickTextsArr;//save the tick texts which at tickMark position.
    private float[] mTickTextsWidth;//save the tick texts width bounds.
    private float[] mTextCenterX;//the text's drawing X anchor
    private float mTickTextY;//the text's drawing Y anchor
    private int mTickTextsSize;
    private Typeface mTextsTypeface;//the tick texts and thumb texts' typeface
    private int mSelectedTextsColor;//the color for the tick texts those thumb swept.
    private int mUnselectedTextsColor;//the color for the tick texts those thumb haven't reach.
    private int mHoveredTextColor;//the color for the tick texts which below/above thumb.
    private CharSequence[] mTickTextsCustomArray;
    //indicator
    private Indicator mIndicator;//the pop window above the seek bar
    private int mIndicatorColor;
    private int mIndicatorTextColor;
    private boolean mIndicatorStayAlways;//true if the indicator didn't dismiss after initial.
    private int mIndicatorTextSize;
    private View mIndicatorContentView;//the view to replace the raw indicator all view
    private View mIndicatorTopContentView;//the view to replace the raw indicator content view
    private int mShowIndicatorType;//different indicator type.
    private String mIndicatorTextFormat;
    //tick marks
    private float[] mTickMarksX;//the tickMark's drawing X anchor
    private int mTicksCount;//the num of tickMarks
    private int mUnSelectedTickMarksColor;//the color for the tickMarks those thumb haven't reach.
    private int mSelectedTickMarksColor;//the color for the tickMarks those thumb swept.
    private float mTickRadius;//the tick's radius
    private Bitmap mUnselectTickMarksBitmap;//the drawable bitmap for tick
    private Bitmap mSelectTickMarksBitmap;//the drawable bitmap for tick
    private Drawable mTickMarksDrawable;
    private int mShowTickMarksType;
    private boolean mTickMarksEndsHide;//true if want to hide the tickMarks which in both side ends of seek bar
    private boolean mTickMarksSweptHide;//true if want to hide the tickMarks which on thumb left.
    private int mTickMarksSize;//the width of tickMark
    //track
    private boolean mTrackRoundedCorners;
    private RectF mProgressTrack;//the background track on the thumb start
    private RectF mBackgroundTrack;//the background track on the thumb ends
    private int mBackgroundTrackSize;
    private int mProgressTrackSize;
    private int mBackgroundTrackColor;
    private int mProgressTrackColor;
    private int[] mSectionTrackColorArray;//save the color for each section tracks.
    private boolean mCustomTrackSectionColorResult;//true to confirm to custom the section track color
    //thumb
    private float mThumbRadius;//the thumb's radius
    private float mThumbTouchRadius;//the thumb's radius when touching
    private Bitmap mThumbBitmap;//the drawable bitmap for thumb
    private int mThumbColor;
    private int mThumbSize;
    private Drawable mThumbDrawable;
    private Bitmap mPressedThumbBitmap;//the bitmap for pressing status
    private int mPressedThumbColor;//the color for pressing status
    //thumb text
    private boolean mShowThumbText;//the place where the thumb text show .
    private float mThumbTextY;//the thumb text's drawing Y anchor
    private int mThumbTextColor;
    private boolean mHideThumb;
    private boolean mAdjustAuto;

    public IndicatorSeekBar(Context context) {
        this(context, null);
    }

    public IndicatorSeekBar(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.mContext = context;
        initAttrs(mContext, attrs);
        initParams();
    }

    @RequiresApi(api = Build.VERSION_CODES.LOLLIPOP)
    public IndicatorSeekBar(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.mContext = context;
        initAttrs(mContext, attrs);
        initParams();
    }



    private void initAttrs(Context context, AttributeSet attrs) {
        IndicatorSeekBarParams indicatorSeekBarParams = new IndicatorSeekBarParams(context);
        if (attrs == null) {
            this.apply(indicatorSeekBarParams);
            return;
        }
        TypedArray ta = context.obtainStyledAttributes(attrs, R.styleable.TE_IndicatorSeekBar);
        //seekBar
        mMax = ta.getFloat(R.styleable.TE_IndicatorSeekBar_te_isb_max, indicatorSeekBarParams.max);
        mMin = ta.getFloat(R.styleable.TE_IndicatorSeekBar_te_isb_min, indicatorSeekBarParams.min);
        mProgress = ta.getFloat(R.styleable.TE_IndicatorSeekBar_te_isb_progress, indicatorSeekBarParams.progress);
        mIsFloatProgress = ta.getBoolean(
                R.styleable.TE_IndicatorSeekBar_te_isb_progress_value_float, indicatorSeekBarParams.progressValueFloat);
        mUserSeekable = ta.getBoolean(
                R.styleable.TE_IndicatorSeekBar_te_isb_user_seekable, indicatorSeekBarParams.userSeekable);
        mClearPadding = ta.getBoolean(
                R.styleable.TE_IndicatorSeekBar_te_isb_clear_default_padding, indicatorSeekBarParams.clearPadding);
        mOnlyThumbDraggable = ta.getBoolean(
                R.styleable.TE_IndicatorSeekBar_te_isb_only_thumb_draggable, indicatorSeekBarParams.onlyThumbDraggable);
        mSeekSmoothly = ta.getBoolean(
                R.styleable.TE_IndicatorSeekBar_te_isb_seek_smoothly, indicatorSeekBarParams.seekSmoothly);
        mR2L = ta.getBoolean(R.styleable.TE_IndicatorSeekBar_te_isb_r2l, indicatorSeekBarParams.r2l);
        //track
        mBackgroundTrackSize = ta.getDimensionPixelSize(R.styleable.TE_IndicatorSeekBar_te_isb_track_background_size,
                indicatorSeekBarParams.trackBackgroundSize);
        mProgressTrackSize = ta.getDimensionPixelSize(R.styleable.TE_IndicatorSeekBar_te_isb_track_progress_size,
                indicatorSeekBarParams.trackProgressSize);
        mBackgroundTrackColor = ta.getColor(R.styleable.TE_IndicatorSeekBar_te_isb_track_background_color,
                indicatorSeekBarParams.trackBackgroundColor);
        mProgressTrackColor = ta.getColor(R.styleable.TE_IndicatorSeekBar_te_isb_track_progress_color,
                indicatorSeekBarParams.trackProgressColor);
        mTrackRoundedCorners = ta.getBoolean(R.styleable.TE_IndicatorSeekBar_te_isb_track_rounded_corners,
                indicatorSeekBarParams.trackRoundedCorners);
        //thumb
        mThumbSize = ta.getDimensionPixelSize(R.styleable.TE_IndicatorSeekBar_te_isb_thumb_size,
                indicatorSeekBarParams.thumbSize);
        mThumbDrawable = ta.getDrawable(R.styleable.TE_IndicatorSeekBar_te_isb_thumb_drawable);
        mAdjustAuto = ta.getBoolean(R.styleable.TE_IndicatorSeekBar_te_isb_thumb_adjust_auto, true);
        initThumbColor(ta.getColorStateList(R.styleable.TE_IndicatorSeekBar_te_isb_thumb_color),
                indicatorSeekBarParams.thumbColor);
        //thumb text
        mShowThumbText = ta.getBoolean(R.styleable.TE_IndicatorSeekBar_te_isb_show_thumb_text,
                indicatorSeekBarParams.showThumbText);
        mThumbTextColor = ta.getColor(R.styleable.TE_IndicatorSeekBar_te_isb_thumb_text_color,
                indicatorSeekBarParams.thumbTextColor);
        //tickMarks
        mTicksCount = ta.getInt(R.styleable.TE_IndicatorSeekBar_te_isb_ticks_count, indicatorSeekBarParams.tickCount);
        mShowTickMarksType = ta.getInt(R.styleable.TE_IndicatorSeekBar_te_isb_show_tick_marks_type,
                indicatorSeekBarParams.showTickMarksType);
        mTickMarksSize = ta.getDimensionPixelSize(R.styleable.TE_IndicatorSeekBar_te_isb_tick_marks_size,
                indicatorSeekBarParams.tickMarksSize);
        initTickMarksColor(ta.getColorStateList(R.styleable.TE_IndicatorSeekBar_te_isb_tick_marks_color),
                indicatorSeekBarParams.tickMarksColor);
        mTickMarksDrawable = ta.getDrawable(R.styleable.TE_IndicatorSeekBar_te_isb_tick_marks_drawable);
        mTickMarksSweptHide = ta.getBoolean(R.styleable.TE_IndicatorSeekBar_te_isb_tick_marks_swept_hide,
                indicatorSeekBarParams.tickMarksSweptHide);
        mTickMarksEndsHide = ta.getBoolean(R.styleable.TE_IndicatorSeekBar_te_isb_tick_marks_ends_hide,
                indicatorSeekBarParams.tickMarksEndsHide);
        //tickTexts
        mShowTickText = ta.getBoolean(R.styleable.TE_IndicatorSeekBar_te_isb_show_tick_texts,
                indicatorSeekBarParams.showTickText);
        mTickTextsSize = ta.getDimensionPixelSize(R.styleable.TE_IndicatorSeekBar_te_isb_tick_texts_size,
                indicatorSeekBarParams.tickTextsSize);
        initTickTextsColor(ta.getColorStateList(R.styleable.TE_IndicatorSeekBar_te_isb_tick_texts_color),
                indicatorSeekBarParams.tickTextsColor);
        mTickTextsCustomArray = ta.getTextArray(R.styleable.TE_IndicatorSeekBar_te_isb_tick_texts_array);
        initTextsTypeface(ta.getInt(R.styleable.TE_IndicatorSeekBar_te_isb_tick_texts_typeface, -1),
                indicatorSeekBarParams.tickTextsTypeFace);
        //indicator
        mShowIndicatorType = ta.getInt(R.styleable.TE_IndicatorSeekBar_te_isb_show_indicator,
                indicatorSeekBarParams.showIndicatorType);
        mIndicatorColor = ta.getColor(R.styleable.TE_IndicatorSeekBar_te_isb_indicator_color,
                indicatorSeekBarParams.indicatorColor);
        mIndicatorTextSize = ta.getDimensionPixelSize(R.styleable.TE_IndicatorSeekBar_te_isb_indicator_text_size,
                indicatorSeekBarParams.indicatorTextSize);
        mIndicatorTextColor = ta.getColor(R.styleable.TE_IndicatorSeekBar_te_isb_indicator_text_color,
                indicatorSeekBarParams.indicatorTextColor);
        int indicatorContentViewId = ta.getResourceId(
                R.styleable.TE_IndicatorSeekBar_te_isb_indicator_content_layout, 0);
        if (indicatorContentViewId > 0) {
            mIndicatorContentView = View.inflate(mContext, indicatorContentViewId, null);
        }
        int indicatorTopContentLayoutId = ta.getResourceId(
                R.styleable.TE_IndicatorSeekBar_te_isb_indicator_top_content_layout, 0);
        if (indicatorTopContentLayoutId > 0) {
            mIndicatorTopContentView = View.inflate(mContext, indicatorTopContentLayoutId, null);
        }
        ta.recycle();
    }

    private void initParams() {
        initProgressRangeValue();
        if (mBackgroundTrackSize > mProgressTrackSize) {
            mBackgroundTrackSize = mProgressTrackSize;
        }
        if (mThumbDrawable == null) {
            mThumbRadius = mThumbSize / 2.0f;
            mThumbTouchRadius = mThumbRadius * 1.2f;
        } else {
            mThumbRadius = Math.min(ScreenUtils.dp2px(mContext, THUMB_MAX_WIDTH), mThumbSize) / 2.0f;
            mThumbTouchRadius = mThumbRadius;
        }
        if (mTickMarksDrawable == null) {
            mTickRadius = mTickMarksSize / 2.0f;
        } else {
            mTickRadius = Math.min(ScreenUtils.dp2px(mContext, THUMB_MAX_WIDTH), mTickMarksSize) / 2.0f;
        }
        mCustomDrawableMaxHeight = Math.max(mThumbTouchRadius, mTickRadius) * 2.0f;
        initStrokePaint();
        measureTickTextsBonds();
        lastProgress = mProgress;

        collectTicksInfo();

        mProgressTrack = new RectF();
        mBackgroundTrack = new RectF();
        initDefaultPadding();
        initIndicatorContentView();
    }

    private void collectTicksInfo() {
        if (mTicksCount < 0 || mTicksCount > 50) {
            throw new IllegalArgumentException(
                    "the Argument: TICK COUNT must be limited between (0-50), Now is " + mTicksCount);
        }
        if (mTicksCount != 0) {
            mTickMarksX = new float[mTicksCount];
            if (mShowTickText) {
                mTextCenterX = new float[mTicksCount];
                mTickTextsWidth = new float[mTicksCount];
            }
            mProgressArr = new float[mTicksCount];
            for (int i = 0; i < mProgressArr.length; i++) {
                mProgressArr[i] = mMin + i * (mMax - mMin) / ((mTicksCount - 1) > 0 ? (mTicksCount - 1) : 1);
            }

        }
    }

    private void initDefaultPadding() {
        if (!mClearPadding) {
            int normalPadding = ScreenUtils.dp2px(mContext, 16);
            if (getPaddingLeft() == 0) {
                setPadding(normalPadding, getPaddingTop(), getPaddingRight(), getPaddingBottom());
            }
            if (getPaddingRight() == 0) {
                setPadding(getPaddingLeft(), getPaddingTop(), normalPadding, getPaddingBottom());
            }
        }
    }

    private void initProgressRangeValue() {
        if (mMax < mMin) {
            throw new IllegalArgumentException("the Argument: MAX's value must be larger than MIN's.");
        }
        if (mProgress < mMin) {
            mProgress = mMin;
        }
        if (mProgress > mMax) {
            mProgress = mMax;
        }
    }

    private void initStrokePaint() {
        if (mStockPaint == null) {
            mStockPaint = new Paint();
        }
        if (mTrackRoundedCorners) {
            mStockPaint.setStrokeCap(Paint.Cap.ROUND);
        }
        mStockPaint.setAntiAlias(true);
        if (mBackgroundTrackSize > mProgressTrackSize) {
            mProgressTrackSize = mBackgroundTrackSize;
        }
    }

    private void measureTickTextsBonds() {
        if (needDrawText()) {
            initTextPaint();
            mTextPaint.setTypeface(mTextsTypeface);
            mTextPaint.getTextBounds("j", 0, 1, mRect);
            mTickTextsHeight = mRect.height() + ScreenUtils.dp2px(mContext, 3);
        }
    }

    private boolean needDrawText() {
        return mShowThumbText || (mTicksCount != 0 && mShowTickText);
    }

    private void initTextPaint() {
        if (mTextPaint == null) {
            mTextPaint = new TextPaint();
            mTextPaint.setAntiAlias(true);
            mTextPaint.setTextAlign(Paint.Align.CENTER);
            mTextPaint.setTextSize(mTickTextsSize);
        }
        if (mRect == null) {
            mRect = new Rect();
        }
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        int height = Math.round(mCustomDrawableMaxHeight + getPaddingTop() + getPaddingBottom());
        setMeasuredDimension(resolveSize(ScreenUtils.dp2px(mContext, 170), widthMeasureSpec),
                height + mTickTextsHeight);
        initSeekBarInfo();
        refreshSeekBarLocation();
    }

    private void initSeekBarInfo() {
        mMeasuredWidth = getMeasuredWidth();
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1) {
            mPaddingLeft = getPaddingLeft();
            mPaddingRight = getPaddingRight();
        } else {
            mPaddingLeft = getPaddingStart();
            mPaddingRight = getPaddingEnd();
        }
        mPaddingTop = getPaddingTop();
        mSeekLength = mMeasuredWidth - mPaddingLeft - mPaddingRight;
        mSeekBlockLength = mSeekLength / (mTicksCount - 1 > 0 ? mTicksCount - 1 : 1);
    }

    private void refreshSeekBarLocation() {
        initTrackLocation();
        //init TickTexts Y Location
        if (needDrawText()) {
            mTextPaint.getTextBounds("j", 0, 1, mRect);
            mTickTextY = mPaddingTop + mCustomDrawableMaxHeight
                    + Math.round(mRect.height() - mTextPaint.descent()) + ScreenUtils.dp2px(mContext, 3);
            mThumbTextY = mTickTextY;
        }
        //init tick's X and text's X location;
        if (mTickMarksX == null) {
            return;
        }
        initTextsArray();
        //adjust thumb auto,so find out the closest progress in the mProgressArr array and replace it.
        //it is not necessary to adjust thumb while count is less than 2.
        if (mTicksCount > 2) {
            mProgress = mProgressArr[getClosestIndex()];
            lastProgress = mProgress;
        }
        refreshThumbCenterXByProgress(mProgress);
    }

    private void initTextsArray() {
        if (mTicksCount == 0) {
            return;
        }
        if (mShowTickText) {
            mTickTextsArr = new String[mTicksCount];
        }
        for (int i = 0; i < mTickMarksX.length; i++) {
            if (mShowTickText) {
                mTickTextsArr[i] = getTickTextByPosition(i);
                mTextPaint.getTextBounds(mTickTextsArr[i], 0, mTickTextsArr[i].length(), mRect);
                mTickTextsWidth[i] = mRect.width();
                mTextCenterX[i] = mPaddingLeft + mSeekBlockLength * i;
            }
            mTickMarksX[i] = mPaddingLeft + mSeekBlockLength * i;
        }
    }

    private void initTrackLocation() {
        if (mR2L) {
            mBackgroundTrack.left = mPaddingLeft;
            mBackgroundTrack.top = mPaddingTop + mThumbTouchRadius;
            //ThumbCenterX
            mBackgroundTrack.right = mPaddingLeft + mSeekLength * (1.0f - (mProgress - mMin) / (getAmplitude()));
            mBackgroundTrack.bottom = mBackgroundTrack.top;
            //ThumbCenterX
            mProgressTrack.left = mBackgroundTrack.right;
            mProgressTrack.top = mBackgroundTrack.top;
            mProgressTrack.right = mMeasuredWidth - mPaddingRight;
            mProgressTrack.bottom = mBackgroundTrack.bottom;
        } else {
            mProgressTrack.left = mPaddingLeft;
            mProgressTrack.top = mPaddingTop + mThumbTouchRadius;
            //ThumbCenterX
            mProgressTrack.right = (mProgress - mMin) * mSeekLength / (getAmplitude()) + mPaddingLeft;
            mProgressTrack.bottom = mProgressTrack.top;
            //ThumbCenterX
            mBackgroundTrack.left = mProgressTrack.right;
            mBackgroundTrack.top = mProgressTrack.bottom;
            mBackgroundTrack.right = mMeasuredWidth - mPaddingRight;
            mBackgroundTrack.bottom = mProgressTrack.bottom;
        }
    }

    private String getTickTextByPosition(int index) {
        if (mTickTextsCustomArray == null) {
            return getProgressString(mProgressArr[index]);
        }
        if (index < mTickTextsCustomArray.length) {
            return String.valueOf(mTickTextsCustomArray[index]);
        }
        return "";
    }

    /**
     * calculate the thumb's centerX by the changing progress.
     */
    private void refreshThumbCenterXByProgress(float progress) {
        //ThumbCenterX
        if (mR2L) {
            mBackgroundTrack.right = mPaddingLeft + mSeekLength * (1.0f - (progress - mMin) / (getAmplitude()));
            mProgressTrack.left = mBackgroundTrack.right;
        } else {
            mProgressTrack.right = (progress - mMin) * mSeekLength / (getAmplitude()) + mPaddingLeft;
            mBackgroundTrack.left = mProgressTrack.right;
        }
    }

    @Override
    protected synchronized void onDraw(Canvas canvas) {
        drawTrack(canvas);
        drawTickMarks(canvas);
        drawTickTexts(canvas);
        drawThumb(canvas);
        drawThumbText(canvas);
    }

    private void drawTrack(Canvas canvas) {
        if (mCustomTrackSectionColorResult) {
            int sectionSize = mTicksCount - 1 > 0 ? mTicksCount - 1 : 1;
            for (int i = 0; i < sectionSize; i++) {
                if (mR2L) {
                    mStockPaint.setColor(mSectionTrackColorArray[sectionSize - i - 1]);
                } else {
                    mStockPaint.setColor(mSectionTrackColorArray[i]);
                }
                float thumbPosFloat = getThumbPosOnTickFloat();
                if (i < thumbPosFloat && thumbPosFloat < (i + 1)) {
                    //the section track include the thumb,
                    // set the ProgressTrackSize for thumb's left side track ,
                    // BGTrackSize for the right's.
                    float thumbCenterX = getThumbCenterX();
                    mStockPaint.setStrokeWidth(getLeftSideTrackSize());
                    canvas.drawLine(mTickMarksX[i], mProgressTrack.top, thumbCenterX,
                            mProgressTrack.bottom, mStockPaint);
                    mStockPaint.setStrokeWidth(getRightSideTrackSize());
                    canvas.drawLine(thumbCenterX, mProgressTrack.top, mTickMarksX[i + 1],
                            mProgressTrack.bottom, mStockPaint);
                } else {
                    if (i < thumbPosFloat) {
                        mStockPaint.setStrokeWidth(getLeftSideTrackSize());
                    } else {
                        mStockPaint.setStrokeWidth(getRightSideTrackSize());
                    }
                    canvas.drawLine(mTickMarksX[i], mProgressTrack.top, mTickMarksX[i + 1],
                            mProgressTrack.bottom, mStockPaint);
                }
            }
        } else {
            //draw progress track
            mStockPaint.setColor(mProgressTrackColor);
            mStockPaint.setStrokeWidth(mProgressTrackSize);
            canvas.drawLine(mProgressTrack.left, mProgressTrack.top, mProgressTrack.right,
                    mProgressTrack.bottom, mStockPaint);
            //draw BG track
            mStockPaint.setColor(mBackgroundTrackColor);
            mStockPaint.setStrokeWidth(mBackgroundTrackSize);
            canvas.drawLine(mBackgroundTrack.left, mBackgroundTrack.top, mBackgroundTrack.right,
                    mBackgroundTrack.bottom, mStockPaint);
        }
    }

    private void drawTickMarks(Canvas canvas) {
        if (mTicksCount == 0 || (mShowTickMarksType == TickMarkType.NONE && mTickMarksDrawable == null)) {
            return;
        }
        float thumbCenterX = getThumbCenterX();
        for (int i = 0; i < mTickMarksX.length; i++) {
            if (mTickMarksSweptHide) {
                if (thumbCenterX >= mTickMarksX[i]) {
                    continue;
                }
            }
            if (mTickMarksEndsHide) {
                if (i == 0 || i == mTickMarksX.length - 1) {
                    continue;
                }
            }
            if (i == getThumbPosOnTick() && mTicksCount > 2 && !mSeekSmoothly) {
                continue;
            }
            float thumbPosFloat = getThumbPosOnTickFloat();
            if (i <= thumbPosFloat) {
                mStockPaint.setColor(getLeftSideTickColor());
            } else {
                mStockPaint.setColor(getRightSideTickColor());
            }
            if (mTickMarksDrawable != null) {
                if (mSelectTickMarksBitmap == null || mUnselectTickMarksBitmap == null) {
                    initTickMarksBitmap();
                }
                if (mSelectTickMarksBitmap == null || mUnselectTickMarksBitmap == null) {
                    //please check your selector drawable's format and correct.
                    throw new IllegalArgumentException("the format of the selector TickMarks drawable is wrong!");
                }
                if (i <= thumbPosFloat) {
                    canvas.drawBitmap(mSelectTickMarksBitmap,
                            mTickMarksX[i] - mUnselectTickMarksBitmap.getWidth() / 2.0f,
                            mProgressTrack.top - mUnselectTickMarksBitmap.getHeight() / 2.0f, mStockPaint);
                } else {
                    canvas.drawBitmap(mUnselectTickMarksBitmap,
                            mTickMarksX[i] - mUnselectTickMarksBitmap.getWidth() / 2.0f,
                            mProgressTrack.top - mUnselectTickMarksBitmap.getHeight() / 2.0f, mStockPaint);
                }
                continue;
            }
            if (mShowTickMarksType == TickMarkType.OVAL) {
                canvas.drawCircle(mTickMarksX[i], mProgressTrack.top, mTickRadius, mStockPaint);
            } else if (mShowTickMarksType == TickMarkType.DIVIDER) {
                int rectWidth = ScreenUtils.dp2px(mContext, 1);
                float dividerTickHeight;
                if (thumbCenterX >= mTickMarksX[i]) {
                    dividerTickHeight = getLeftSideTrackSize();
                } else {
                    dividerTickHeight = getRightSideTrackSize();
                }
                canvas.drawRect(mTickMarksX[i] - rectWidth, mProgressTrack.top - dividerTickHeight / 2.0f,
                        mTickMarksX[i] + rectWidth,
                        mProgressTrack.top + dividerTickHeight / 2.0f, mStockPaint);
            } else if (mShowTickMarksType == TickMarkType.SQUARE) {
                canvas.drawRect(mTickMarksX[i] - mTickMarksSize / 2.0f,
                        mProgressTrack.top - mTickMarksSize / 2.0f, mTickMarksX[i] + mTickMarksSize / 2.0f,
                        mProgressTrack.top + mTickMarksSize / 2.0f, mStockPaint);
            }
        }
    }

    private void drawTickTexts(Canvas canvas) {
        if (mTickTextsArr == null) {
            return;
        }
        for (int i = 0; i < mTickTextsArr.length; i++) {
            if (mShowBothTickTextsOnly) {
                if (i != 0 && i != mTickTextsArr.length - 1) {
                    continue;
                }
            }
            float thumbPosFloat = getThumbPosOnTickFloat();
            if (i == getThumbPosOnTick() && i == thumbPosFloat) {
                mTextPaint.setColor(mHoveredTextColor);
            } else if (i < thumbPosFloat) {
                mTextPaint.setColor(getLeftSideTickTextsColor());
            } else {
                mTextPaint.setColor(getRightSideTickTextsColor());
            }
            int index = i;
            if (mR2L) {
                index = mTickTextsArr.length - i - 1;
            }
            if (i == 0) {
                canvas.drawText(mTickTextsArr[index], mTextCenterX[i] + mTickTextsWidth[index] / 2.0f,
                        mTickTextY, mTextPaint);
            } else if (i == mTickTextsArr.length - 1) {
                canvas.drawText(mTickTextsArr[index], mTextCenterX[i] - mTickTextsWidth[index] / 2.0f,
                        mTickTextY, mTextPaint);
            } else {
                canvas.drawText(mTickTextsArr[index], mTextCenterX[i], mTickTextY, mTextPaint);
            }
        }
    }

    private void drawThumb(Canvas canvas) {
        if (mHideThumb) {
            return;
        }
        float thumbCenterX = getThumbCenterX();
        if (mThumbDrawable != null) {
            if (mThumbBitmap == null || mPressedThumbBitmap == null) {
                initThumbBitmap();
            }
            if (mThumbBitmap == null || mPressedThumbBitmap == null) {
                //please check your selector drawable's format and correct.
                throw new IllegalArgumentException("the format of the selector thumb drawable is wrong!");
            }
            mStockPaint.setAlpha(255);
            if (mIsTouching) {
                canvas.drawBitmap(mPressedThumbBitmap, thumbCenterX - mPressedThumbBitmap.getWidth() / 2.0f,
                        mProgressTrack.top - mPressedThumbBitmap.getHeight() / 2.0f, mStockPaint);
            } else {
                canvas.drawBitmap(mThumbBitmap, thumbCenterX - mThumbBitmap.getWidth() / 2.0f,
                        mProgressTrack.top - mThumbBitmap.getHeight() / 2.0f, mStockPaint);
            }
        } else {
            if (mIsTouching) {
                mStockPaint.setColor(mPressedThumbColor);
            } else {
                mStockPaint.setColor(mThumbColor);
            }
            canvas.drawCircle(thumbCenterX, mProgressTrack.top, mIsTouching
                    ? mThumbTouchRadius : mThumbRadius, mStockPaint);
        }
    }

    private void drawThumbText(Canvas canvas) {
        if (!mShowThumbText || (mShowTickText && mTicksCount > 2)) {
            return;
        }
        mTextPaint.setColor(mThumbTextColor);
        canvas.drawText(getProgressString(mProgress), getThumbCenterX(), mThumbTextY, mTextPaint);
    }

    private float getThumbCenterX() {
        if (mR2L) {
            return mBackgroundTrack.right;
        }
        return mProgressTrack.right;
    }

    private int getLeftSideTickColor() {
        if (mR2L) {
            return mUnSelectedTickMarksColor;
        }
        return mSelectedTickMarksColor;
    }

    private int getRightSideTickColor() {
        if (mR2L) {
            return mSelectedTickMarksColor;
        }
        return mUnSelectedTickMarksColor;
    }

    private int getLeftSideTickTextsColor() {
        if (mR2L) {
            return mUnselectedTextsColor;
        }
        return mSelectedTextsColor;
    }

    private int getRightSideTickTextsColor() {
        if (mR2L) {
            return mSelectedTextsColor;
        }
        return mUnselectedTextsColor;
    }

    /**
     * get the track size which on the thumb left in R2L/L2R case.
     */
    private int getLeftSideTrackSize() {
        if (mR2L) {
            return mBackgroundTrackSize;
        }
        return mProgressTrackSize;
    }

    /**
     * get the track size which on the thumb right in R2L/L2R case.
     */
    private int getRightSideTrackSize() {
        if (mR2L) {
            return mProgressTrackSize;
        }
        return mBackgroundTrackSize;
    }

    private int getThumbPosOnTick() {
        if (mTicksCount != 0) {
            return Math.round((getThumbCenterX() - mPaddingLeft) / mSeekBlockLength);
        }
        //when tick count = 0 ; seek bar has not tick(continuous series), return 0;
        return 0;
    }

    private float getThumbPosOnTickFloat() {
        if (mTicksCount != 0) {
            return (getThumbCenterX() - mPaddingLeft) / mSeekBlockLength;
        }
        return 0;
    }

    private int getHeightByRatio(Drawable drawable, int width) {
        int intrinsicWidth = drawable.getIntrinsicWidth();
        int intrinsicHeight = drawable.getIntrinsicHeight();
        return Math.round(1.0f * width * intrinsicHeight / intrinsicWidth);
    }

    private Bitmap getDrawBitmap(Drawable drawable, boolean isThumb) {
        if (drawable == null) {
            return null;
        }
        int width;
        int height;
        int maxRange = ScreenUtils.dp2px(mContext, THUMB_MAX_WIDTH);
        int intrinsicWidth = drawable.getIntrinsicWidth();
        if (intrinsicWidth > maxRange) {
            if (isThumb) {
                width = mThumbSize;
            } else {
                width = mTickMarksSize;
            }
            height = getHeightByRatio(drawable, width);

            if (width > maxRange) {
                width = maxRange;
                height = getHeightByRatio(drawable, width);
            }
        } else {
            width = drawable.getIntrinsicWidth();
            height = drawable.getIntrinsicHeight();
        }

        Bitmap bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, canvas.getWidth(), canvas.getHeight());
        drawable.draw(canvas);
        return bitmap;
    }

    private void initThumbColor(ColorStateList colorStateList, int defaultColor) {
        //if you didn't set the thumb color, set a default color.
        if (colorStateList == null) {
            mThumbColor = defaultColor;
            mPressedThumbColor = mThumbColor;
            return;
        }
        int[][] states = null;
        int[] colors = null;
        Class<? extends ColorStateList> aClass = colorStateList.getClass();
        try {
            Field[] f = aClass.getDeclaredFields();
            for (Field field : f) {
                field.setAccessible(true);
                if ("mStateSpecs".equals(field.getName())) {
                    states = (int[][]) field.get(colorStateList);
                }
                if ("mColors".equals(field.getName())) {
                    colors = (int[]) field.get(colorStateList);
                }
            }
            if (states == null || colors == null) {
                return;
            }
        } catch (Exception e) {
            throw new RuntimeException("Something wrong happened when parseing thumb selector color.");
        }
        if (states.length == 1) {
            mThumbColor = colors[0];
            mPressedThumbColor = mThumbColor;
        } else if (states.length == 2) {
            for (int i = 0; i < states.length; i++) {
                int[] attr = states[i];
                if (attr.length == 0) {
                    mPressedThumbColor = colors[i];
                    continue;
                }
                switch (attr[0]) {
                    case android.R.attr.state_pressed:
                        mThumbColor = colors[i];
                        break;
                    default:
                        //the color selector file was set by a wrong format , please see above to correct.
                        throw new IllegalArgumentException("the selector color file you set for the argument: "
                                + "isb_thumb_color is in wrong format.");
                }
            }
        } else {
            //the color selector file was set by a wrong format , please see above to correct.
            throw new IllegalArgumentException("the selector color file you set for the argument: isb_thumb_color "
                    + "is in wrong format.");
        }

    }

    private void initTickMarksColor(ColorStateList colorStateList, int defaultColor) {
        //if you didn't set the tick's text color, set a default selector color file.
        if (colorStateList == null) {
            mSelectedTickMarksColor = defaultColor;
            mUnSelectedTickMarksColor = mSelectedTickMarksColor;
            return;
        }
        int[][] states = null;
        int[] colors = null;
        Class<? extends ColorStateList> aClass = colorStateList.getClass();
        try {
            Field[] f = aClass.getDeclaredFields();
            for (Field field : f) {
                field.setAccessible(true);
                if ("mStateSpecs".equals(field.getName())) {
                    states = (int[][]) field.get(colorStateList);
                }
                if ("mColors".equals(field.getName())) {
                    colors = (int[]) field.get(colorStateList);
                }
            }
            if (states == null || colors == null) {
                return;
            }
        } catch (Exception e) {
            throw new RuntimeException("Something wrong happened when parsing thumb selector color." + e.getMessage());
        }
        if (states.length == 1) {
            mSelectedTickMarksColor = colors[0];
            mUnSelectedTickMarksColor = mSelectedTickMarksColor;
        } else if (states.length == 2) {
            for (int i = 0; i < states.length; i++) {
                int[] attr = states[i];
                if (attr.length == 0) {
                    mUnSelectedTickMarksColor = colors[i];
                    continue;
                }
                switch (attr[0]) {
                    case android.R.attr.state_selected:
                        mSelectedTickMarksColor = colors[i];
                        break;
                    default:
                        //the color selector file was set by a wrong format , please see above to correct.
                        throw new IllegalArgumentException("the selector color file you set for the argument: "
                                + "isb_tick_marks_color is in wrong format.");
                }
            }
        } else {
            //the color selector file was set by a wrong format , please see above to correct.
            throw new IllegalArgumentException("the selector color file you set for the argument: "
                    + "isb_tick_marks_color is in wrong format.");
        }
    }

    private void initTickTextsColor(ColorStateList colorStateList, int defaultColor) {
        if (colorStateList == null) {
            mUnselectedTextsColor = defaultColor;
            mSelectedTextsColor = mUnselectedTextsColor;
            mHoveredTextColor = mUnselectedTextsColor;
            return;
        }
        int[][] states = null;
        int[] colors = null;
        Class<? extends ColorStateList> aClass = colorStateList.getClass();
        try {
            Field[] f = aClass.getDeclaredFields();
            for (Field field : f) {
                field.setAccessible(true);
                if ("mStateSpecs".equals(field.getName())) {
                    states = (int[][]) field.get(colorStateList);
                }
                if ("mColors".equals(field.getName())) {
                    colors = (int[]) field.get(colorStateList);
                }
            }
            if (states == null || colors == null) {
                return;
            }
        } catch (Exception e) {
            throw new RuntimeException("Something wrong happened when parseing thumb selector color.");
        }
        if (states.length == 1) {
            mUnselectedTextsColor = colors[0];
            mSelectedTextsColor = mUnselectedTextsColor;
            mHoveredTextColor = mUnselectedTextsColor;
        } else if (states.length == 3) {
            for (int i = 0; i < states.length; i++) {
                int[] attr = states[i];
                if (attr.length == 0) {
                    mUnselectedTextsColor = colors[i];
                    continue;
                }
                switch (attr[0]) {
                    case android.R.attr.state_selected:
                        mSelectedTextsColor = colors[i];
                        break;
                    case android.R.attr.state_hovered:
                        mHoveredTextColor = colors[i];
                        break;
                    default:
                        //the color selector file was set by a wrong format , please see above to correct.
                        throw new IllegalArgumentException("the selector color file you set for the argument: "
                                + "isb_tick_texts_color is in wrong format.");
                }
            }
        } else {
            //the color selector file was set by a wrong format , please see above to correct.
            throw new IllegalArgumentException("the selector color file you set for the argument:"
                    + " isb_tick_texts_color is in wrong format.");
        }
    }

    private void initTextsTypeface(int typeface, Typeface defaultTypeface) {
        switch (typeface) {
            case 0:
                mTextsTypeface = Typeface.DEFAULT;
                break;
            case 1:
                mTextsTypeface = Typeface.MONOSPACE;
                break;
            case 2:
                mTextsTypeface = Typeface.SANS_SERIF;
                break;
            case 3:
                mTextsTypeface = Typeface.SERIF;
                break;
            default:
                if (defaultTypeface == null) {
                    mTextsTypeface = Typeface.DEFAULT;
                    break;
                }
                mTextsTypeface = defaultTypeface;
                break;
        }
    }

    private void initThumbBitmap() {
        if (mThumbDrawable == null) {
            return;
        }
        if (mThumbDrawable instanceof StateListDrawable) {
            try {
                StateListDrawable listDrawable = (StateListDrawable) mThumbDrawable;
                Class<? extends StateListDrawable> aClass = listDrawable.getClass();
                int stateCount = (int) aClass.getMethod("getStateCount").invoke(listDrawable);
                if (stateCount == 2) {
                    Method getStateSet = aClass.getMethod("getStateSet", int.class);
                    Method getStateDrawable = aClass.getMethod("getStateDrawable", int.class);
                    for (int i = 0; i < stateCount; i++) {
                        int[] stateSet = (int[]) getStateSet.invoke(listDrawable, i);
                        if (stateSet.length > 0) {
                            if (stateSet[0] == android.R.attr.state_pressed) {
                                Drawable stateDrawable = (Drawable) getStateDrawable.invoke(listDrawable, i);
                                mPressedThumbBitmap = getDrawBitmap(stateDrawable, true);
                            } else {
                                //please check your selector drawable's format, please see above to correct.
                                throw new IllegalArgumentException(
                                        "the state of the selector thumb drawable is wrong!");
                            }
                        } else {
                            Drawable stateDrawable = (Drawable) getStateDrawable.invoke(listDrawable, i);
                            mThumbBitmap = getDrawBitmap(stateDrawable, true);
                        }
                    }
                } else {
                    //please check your selector drawable's format, please see above to correct.
                    throw new IllegalArgumentException("the format of the selector thumb drawable is wrong!");
                }
            } catch (Exception e) {
                mThumbBitmap = getDrawBitmap(mThumbDrawable, true);
                mPressedThumbBitmap = mThumbBitmap;
            }
        } else {
            mThumbBitmap = getDrawBitmap(mThumbDrawable, true);
            mPressedThumbBitmap = mThumbBitmap;
        }
    }

    private void initTickMarksBitmap() {
        if (mTickMarksDrawable instanceof StateListDrawable) {
            StateListDrawable listDrawable = (StateListDrawable) mTickMarksDrawable;
            try {
                Class<? extends StateListDrawable> aClass = listDrawable.getClass();
                Method getStateCount = aClass.getMethod("getStateCount");
                int stateCount = (int) getStateCount.invoke(listDrawable);
                if (stateCount == 2) {
                    Method getStateSet = aClass.getMethod("getStateSet", int.class);
                    Method getStateDrawable = aClass.getMethod("getStateDrawable", int.class);
                    for (int i = 0; i < stateCount; i++) {
                        int[] stateSet = (int[]) getStateSet.invoke(listDrawable, i);
                        if (stateSet.length > 0) {
                            if (stateSet[0] == android.R.attr.state_selected) {
                                Drawable stateDrawable = (Drawable) getStateDrawable.invoke(listDrawable, i);
                                mSelectTickMarksBitmap = getDrawBitmap(stateDrawable, false);
                            } else {
                                //please check your selector drawable's format, please see above to correct.
                                throw new IllegalArgumentException(
                                        "the state of the selector TickMarks drawable is wrong!");
                            }
                        } else {
                            Drawable stateDrawable = (Drawable) getStateDrawable.invoke(listDrawable, i);
                            mUnselectTickMarksBitmap = getDrawBitmap(stateDrawable, false);
                        }
                    }
                } else {
                    //please check your selector drawable's format, please see above to correct.
                    throw new IllegalArgumentException("the format of the selector TickMarks drawable is wrong!");
                }
            } catch (Exception e) {
                mUnselectTickMarksBitmap = getDrawBitmap(mTickMarksDrawable, false);
                mSelectTickMarksBitmap = mUnselectTickMarksBitmap;
            }
        } else {
            mUnselectTickMarksBitmap = getDrawBitmap(mTickMarksDrawable, false);
            mSelectTickMarksBitmap = mUnselectTickMarksBitmap;
        }

    }

    @Override
    public void setEnabled(boolean enabled) {
        if (enabled == isEnabled()) {
            return;
        }
        super.setEnabled(enabled);
        if (isEnabled()) {
            setAlpha(1.0f);
            if (mIndicatorStayAlways) {
                mIndicatorContentView.setAlpha(1.0f);
            }
        } else {
            setAlpha(0.3f);
            if (mIndicatorStayAlways) {
                mIndicatorContentView.setAlpha(0.3f);
            }
        }
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);
        post(new Runnable() {
            @Override
            public void run() {
                requestLayout();
            }
        });
    }

    @Override
    public boolean dispatchTouchEvent(MotionEvent event) {
        ViewParent parent = getParent();
        if (parent == null) {
            return super.dispatchTouchEvent(event);
        }
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                parent.requestDisallowInterceptTouchEvent(true);
                break;
            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_CANCEL:
                parent.requestDisallowInterceptTouchEvent(false);
                break;
            default:
                break;
        }
        return super.dispatchTouchEvent(event);
    }

    @Override
    public boolean performClick() {
        return super.performClick();
    }

    @Override
    protected Parcelable onSaveInstanceState() {
        Bundle bundle = new Bundle();
        bundle.putParcelable("isb_instance_state", super.onSaveInstanceState());
        bundle.putFloat("isb_progress", mProgress);
        return bundle;
    }

    @Override
    protected void onRestoreInstanceState(Parcelable state) {
        if (state instanceof Bundle) {
            Bundle bundle = (Bundle) state;
            setProgress(bundle.getFloat("isb_progress"));
            super.onRestoreInstanceState(bundle.getParcelable("isb_instance_state"));
            return;
        }
        super.onRestoreInstanceState(state);
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (!mUserSeekable || !isEnabled()) {
            return false;
        }
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                performClick();
                float mX = event.getX();
                if (isTouchSeekBar(mX, event.getY())) {
                    if ((mOnlyThumbDraggable && !isTouchThumb(mX))) {
                        return false;
                    }
                    mIsTouching = true;
                    if (mSeekChangeListener != null) {
                        mSeekChangeListener.onStartTrackingTouch(this);
                    }
                    refreshSeekBar(event);
                    return true;
                }
                break;
            case MotionEvent.ACTION_MOVE:
                refreshSeekBar(event);
                break;
            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_CANCEL:
                mIsTouching = false;
                if (mSeekChangeListener != null) {
                    mSeekChangeListener.onStopTrackingTouch(this);
                }
                if (!autoAdjustThumb()) {
                    invalidate();
                }
                if (mIndicator != null) {
                    mIndicator.hide();
                }
                break;
            default:
                break;
        }
        return super.onTouchEvent(event);
    }

    private void refreshSeekBar(MotionEvent event) {
        refreshThumbCenterXByProgress(calculateProgress(calculateTouchX(adjustTouchX(event))));
        setSeekListener(true);
        invalidate();
        updateIndicator();
    }

    private boolean progressChange() {
        if (mIsFloatProgress) {
            return lastProgress != mProgress;
        } else {
            return Math.round(lastProgress) != Math.round(mProgress);
        }
    }

    private float adjustTouchX(MotionEvent event) {
        float mTouchXCache;
        if (event.getX() < mPaddingLeft) {
            mTouchXCache = mPaddingLeft;
        } else if (event.getX() > mMeasuredWidth - mPaddingRight) {
            mTouchXCache = mMeasuredWidth - mPaddingRight;
        } else {
            mTouchXCache = event.getX();
        }
        return mTouchXCache;
    }

    private float calculateProgress(float touchX) {
        lastProgress = mProgress;
        mProgress = mMin + (getAmplitude()) * (touchX - mPaddingLeft) / mSeekLength;
        return mProgress;
    }

    private float calculateTouchX(float touchX) {
        float touchXTemp = touchX;
        //make sure the seek bar to seek smoothly always
        // while the tick's count is less than 3(tick's count is 1 or 2.).
        if (mTicksCount > 2 && !mSeekSmoothly) {
            int touchBlockSize = Math.round((touchX - mPaddingLeft) / mSeekBlockLength);
            touchXTemp = mSeekBlockLength * touchBlockSize + mPaddingLeft;
        }
        if (mR2L) {
            return mSeekLength - touchXTemp + 2 * mPaddingLeft;
        }
        return touchXTemp;
    }

    private boolean isTouchSeekBar(float mX, float mY) {
        if (mFaultTolerance == -1) {
            mFaultTolerance = ScreenUtils.dp2px(mContext, 5);
        }
        boolean inWidthRange = mX >= (mPaddingLeft - 2 * mFaultTolerance)
                && mX <= (mMeasuredWidth - mPaddingRight + 2 * mFaultTolerance);
        boolean inHeightRange = mY >= mProgressTrack.top - mThumbTouchRadius
                - mFaultTolerance && mY <= mProgressTrack.top + mThumbTouchRadius + mFaultTolerance;
        return inWidthRange && inHeightRange;
    }

    private boolean isTouchThumb(float mX) {
        float rawTouchX;
        refreshThumbCenterXByProgress(mProgress);
        if (mR2L) {
            rawTouchX = mBackgroundTrack.right;
        } else {
            rawTouchX = mProgressTrack.right;
        }
        return rawTouchX - mThumbSize / 2f <= mX && mX <= rawTouchX + mThumbSize / 2f;
    }

    private void updateIndicator() {
        if (mIndicatorStayAlways) {
            updateStayIndicator();
        } else {
            if (mIndicator == null) {
                return;
            }
            mIndicator.iniPop();
            if (mIndicator.isShowing()) {
                mIndicator.update(getThumbCenterX());
            } else {
                mIndicator.show(getThumbCenterX());
            }
        }
    }

    private void initIndicatorContentView() {
        if (mShowIndicatorType == IndicatorType.NONE) {
            return;
        }
        if (mIndicator == null) {
            mIndicator = new Indicator(mContext,
                    this,
                    mIndicatorColor,
                    mShowIndicatorType,
                    mIndicatorTextSize,
                    mIndicatorTextColor,
                    mIndicatorContentView,
                    mIndicatorTopContentView);
            this.mIndicatorContentView = mIndicator.getInsideContentView();
        }

    }

    private void updateStayIndicator() {
        if (!mIndicatorStayAlways || mIndicator == null) {
            return;
        }
        mIndicator.setProgressTextView(getIndicatorTextString());
        mIndicatorContentView.measure(0, 0);
        int measuredWidth = mIndicatorContentView.getMeasuredWidth();
        float thumbCenterX = getThumbCenterX();

        if (mScreenWidth == -1) {
            DisplayMetrics metric = new DisplayMetrics();
            WindowManager systemService = (WindowManager) mContext.getSystemService(Context.WINDOW_SERVICE);
            if (systemService != null) {
                systemService.getDefaultDisplay().getMetrics(metric);
                mScreenWidth = metric.widthPixels;
            }
        }
        int indicatorOffset;
        int arrowOffset;
        if (measuredWidth / 2 + thumbCenterX > mMeasuredWidth) {
            indicatorOffset = mMeasuredWidth - measuredWidth;
            arrowOffset = (int) (thumbCenterX - indicatorOffset - measuredWidth / 2);
        } else if (thumbCenterX - measuredWidth / 2 < 0) {
            indicatorOffset = 0;
            arrowOffset = -(int) (measuredWidth / 2 - thumbCenterX);
        } else {
            indicatorOffset = (int) (getThumbCenterX() - measuredWidth / 2);
            arrowOffset = 0;
        }

        mIndicator.updateIndicatorLocation(indicatorOffset);
        mIndicator.updateArrowViewLocation(arrowOffset);
    }

    private boolean autoAdjustThumb() {
        if (mTicksCount < 3 || !mSeekSmoothly) {
            return false;
        }
        if (!mAdjustAuto) {
            return false;
        }
        final int closestIndex = getClosestIndex();
        final float touchUpProgress = mProgress;
        ValueAnimator animator =
                ValueAnimator.ofFloat(0, Math.abs(touchUpProgress - mProgressArr[closestIndex]));
        animator.start();
        animator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
            @Override
            public void onAnimationUpdate(ValueAnimator animation) {
                lastProgress = mProgress;
                if (touchUpProgress - mProgressArr[closestIndex] > 0) {
                    mProgress = touchUpProgress - (Float) animation.getAnimatedValue();
                } else {
                    mProgress = touchUpProgress + (Float) animation.getAnimatedValue();
                }
                refreshThumbCenterXByProgress(mProgress);
                //the auto adjust was happened after user touched up, so from user is false.
                setSeekListener(false);
                if (mIndicator != null && mIndicatorStayAlways) {
                    mIndicator.refreshProgressText();
                    updateStayIndicator();
                }
                invalidate();
            }
        });
        return true;
    }

    /**
     * transfer the progress value to string type
     */
    private String getProgressString(float progress) {
        if (mIsFloatProgress) {
            return FormatUtils.fastFormat(progress, mScale);
        } else {
            return String.valueOf(Math.round(progress));
        }
    }

    private int getClosestIndex() {
        int closestIndex = 0;
        float amplitude = Math.abs(mMax - mMin);
        for (int i = 0; i < mProgressArr.length; i++) {
            float amplitudeTemp = Math.abs(mProgressArr[i] - mProgress);
            if (amplitudeTemp <= amplitude) {
                amplitude = amplitudeTemp;
                closestIndex = i;
            }
        }
        return closestIndex;
    }

    private float getAmplitude() {
        return (mMax - mMin) > 0 ? (mMax - mMin) : 1;
    }

    private void setSeekListener(boolean formUser) {
        if (mSeekChangeListener == null) {
            return;
        }
        if (progressChange()) {
            mSeekChangeListener.onSeeking(collectParams(formUser));
        }
    }

    private SeekParams collectParams(boolean formUser) {
        if (mSeekParams == null) {
            mSeekParams = new SeekParams(this);
        }
        mSeekParams.progress = getProgress();
        mSeekParams.progressFloat = getProgressFloat();
        mSeekParams.fromUser = formUser;
        //for discrete series seek bar
        if (mTicksCount > 2) {
            int rawThumbPos = getThumbPosOnTick();
            if (mShowTickText && mTickTextsArr != null) {
                mSeekParams.tickText = mTickTextsArr[rawThumbPos];
            }
            if (mR2L) {
                mSeekParams.thumbPosition = mTicksCount - rawThumbPos - 1;
            } else {
                mSeekParams.thumbPosition = rawThumbPos;
            }
        }
        return mSeekParams;
    }

    private void apply(IndicatorSeekBarParams indicatorSeekBarParams) {
        //seek bar
        this.mMax = indicatorSeekBarParams.max;
        this.mMin = indicatorSeekBarParams.min;
        this.mProgress = indicatorSeekBarParams.progress;
        this.mIsFloatProgress = indicatorSeekBarParams.progressValueFloat;
        this.mTicksCount = indicatorSeekBarParams.tickCount;
        this.mSeekSmoothly = indicatorSeekBarParams.seekSmoothly;
        this.mR2L = indicatorSeekBarParams.r2l;
        this.mUserSeekable = indicatorSeekBarParams.userSeekable;
        this.mClearPadding = indicatorSeekBarParams.clearPadding;
        this.mOnlyThumbDraggable = indicatorSeekBarParams.onlyThumbDraggable;
        //indicator
        this.mShowIndicatorType = indicatorSeekBarParams.showIndicatorType;
        this.mIndicatorColor = indicatorSeekBarParams.indicatorColor;
        this.mIndicatorTextColor = indicatorSeekBarParams.indicatorTextColor;
        this.mIndicatorTextSize = indicatorSeekBarParams.indicatorTextSize;
        this.mIndicatorContentView = indicatorSeekBarParams.indicatorContentView;
        this.mIndicatorTopContentView = indicatorSeekBarParams.indicatorTopContentView;
        //track
        this.mBackgroundTrackSize = indicatorSeekBarParams.trackBackgroundSize;
        this.mBackgroundTrackColor = indicatorSeekBarParams.trackBackgroundColor;
        this.mProgressTrackSize = indicatorSeekBarParams.trackProgressSize;
        this.mProgressTrackColor = indicatorSeekBarParams.trackProgressColor;
        this.mTrackRoundedCorners = indicatorSeekBarParams.trackRoundedCorners;
        //thumb
        this.mThumbSize = indicatorSeekBarParams.thumbSize;
        this.mThumbDrawable = indicatorSeekBarParams.thumbDrawable;
        this.mThumbTextColor = indicatorSeekBarParams.thumbTextColor;
        initThumbColor(indicatorSeekBarParams.thumbColorStateList, indicatorSeekBarParams.thumbColor);
        this.mShowThumbText = indicatorSeekBarParams.showThumbText;
        //tickMarks
        this.mShowTickMarksType = indicatorSeekBarParams.showTickMarksType;
        this.mTickMarksSize = indicatorSeekBarParams.tickMarksSize;
        this.mTickMarksDrawable = indicatorSeekBarParams.tickMarksDrawable;
        this.mTickMarksEndsHide = indicatorSeekBarParams.tickMarksEndsHide;
        this.mTickMarksSweptHide = indicatorSeekBarParams.tickMarksSweptHide;
        initTickMarksColor(indicatorSeekBarParams.tickMarksColorStateList, indicatorSeekBarParams.tickMarksColor);
        //tickTexts
        this.mShowTickText = indicatorSeekBarParams.showTickText;
        this.mTickTextsSize = indicatorSeekBarParams.tickTextsSize;
        this.mTickTextsCustomArray = indicatorSeekBarParams.tickTextsCustomArray;
        this.mTextsTypeface = indicatorSeekBarParams.tickTextsTypeFace;
        initTickTextsColor(indicatorSeekBarParams.tickTextsColorStateList, indicatorSeekBarParams.tickTextsColor);
    }

    /**
     * first showing when initial for indicator stay always.
     */
    void showStayIndicator() {
        mIndicatorContentView.setVisibility(INVISIBLE);
        postDelayed(new Runnable() {
            @Override
            public void run() {
                Animation animation = new AlphaAnimation(0.1f, 1.0f);
                animation.setDuration(180);
                mIndicatorContentView.setAnimation(animation);
                updateStayIndicator();
                mIndicatorContentView.setVisibility(VISIBLE);
            }
        }, 300);
    }

    /**
     * @param indicatorStayAlways IndicatorStayLayout call this, always true.
     */
    void setIndicatorStayAlways(boolean indicatorStayAlways) {
        this.mIndicatorStayAlways = indicatorStayAlways;
    }

    View getIndicatorContentView() {
        return mIndicatorContentView;
    }

    String getIndicatorTextString() {
        if (mIndicatorTextFormat != null && mIndicatorTextFormat.contains(FORMAT_TICK_TEXT)) {
            if (mTicksCount > 2 && mTickTextsArr != null) {
                return mIndicatorTextFormat.replace(FORMAT_TICK_TEXT, mTickTextsArr[getThumbPosOnTick()]);
            }
        } else if (mIndicatorTextFormat != null && mIndicatorTextFormat.contains(FORMAT_PROGRESS)) {
            return mIndicatorTextFormat.replace(FORMAT_PROGRESS, getProgressString(mProgress));
        }
        return getProgressString(mProgress);
    }

    /*------------------API START-------------------*/

    /**
     * call this to new a default params.
     *
     * @param context context environment
     * @return IndicatorSeekBarParams
     */
    public static IndicatorSeekBarParams with(@NonNull Context context) {
        return new IndicatorSeekBarParams(context);
    }

    /**
     * get current indicator
     *
     * @return the indicator
     */
    public Indicator getIndicator() {
        return mIndicator;
    }

    /**
     * get the tick' count
     *
     * @return tick' count
     */
    public int getTickCount() {
        return mTicksCount;
    }

    /**
     * Get the seek bar's current level of progress in float type.
     *
     * @return current progress in float type.
     */
    public synchronized float getProgressFloat() {
        BigDecimal bigDecimal = BigDecimal.valueOf(mProgress);
        return bigDecimal.setScale(mScale, BigDecimal.ROUND_HALF_UP).floatValue();
    }

    /**
     * Get the seek bar's current level of progress in int type.
     *
     * @return progress in int type.
     */
    public int getProgress() {
        return Math.round(mProgress);
    }

    /**
     * @return the upper limit of this seek bar's range.
     */
    public float getMax() {
        return mMax;
    }

    /**
     * the lower limit of this seek bar's range.
     *
     * @return the seek bar min value
     */
    public float getMin() {
        return mMin;
    }

    /**
     * the listener to listen the seeking params changing.
     *
     * @return seeking listener.
     */
    public OnSeekChangeListener getOnSeekChangeListener() {
        return mSeekChangeListener;
    }

    /**
     * Sets the current progress to the specified value.also,
     * if the seek bar's tick'count is larger than 2,the progress will adjust to the closest tick's progress auto.
     *
     * @param progress a new progress value , if the new progress is less than min ,
     *                 it will set to min;
     *                 if over max ,will be max.
     */
    public synchronized void setProgress(float progress) {
        lastProgress = mProgress;
        mProgress = progress < mMin ? mMin : (progress > mMax ? mMax : progress);
        //adjust to the closest tick's progress
        if ((!mSeekSmoothly) && mTicksCount > 2) {
            mProgress = mProgressArr[getClosestIndex()];
        }
        setSeekListener(false);
        refreshThumbCenterXByProgress(mProgress);
        postInvalidate();
        updateStayIndicator();
    }

    /**
     * Set the upper range of the seek bar
     *
     * @param max the upper range of this progress bar.
     */
    public synchronized void setMax(float max) {
        this.mMax = Math.max(mMin, max);
        initProgressRangeValue();
        collectTicksInfo();
        refreshSeekBarLocation();
        invalidate();
        updateStayIndicator();
    }

    /**
     * Set the min value for SeekBar
     *
     * @param min the min value , if is larger than max, will set to max.
     */
    public synchronized void setMin(float min) {
        this.mMin = Math.min(mMax, min);
        initProgressRangeValue();
        collectTicksInfo();
        refreshSeekBarLocation();
        invalidate();
        updateStayIndicator();
    }

    /**
     * compat app local change
     *
     * @param isR2L True if see form right to left on the screen.
     */
    public void setR2L(boolean isR2L) {
        this.mR2L = isR2L;
        requestLayout();
        invalidate();
        updateStayIndicator();
    }

    /**
     * Set a new thumb drawable.
     *
     * @param drawable the drawable for thumb,selector drawable is ok.
     *                 selector format:
     */
    //<?xml version="1.0" encoding="utf-8"?>
    //<selector xmlns:android="http://schemas.android.com/apk/res/android">
    //<item android:drawable="@drawable/ic_launcher" android:state_pressed="true" />
    // <!--this drawable is for thumb when pressing-->
    //<item android:drawable="@drawable/ic_launcher_round" />  < !--for thumb when normal-->
    //</selector>
    public void setThumbDrawable(Drawable drawable) {
        if (drawable == null) {
            this.mThumbDrawable = null;
            this.mThumbBitmap = null;
            this.mPressedThumbBitmap = null;
        } else {
            this.mThumbDrawable = drawable;
            this.mThumbRadius = Math.min(ScreenUtils.dp2px(mContext, THUMB_MAX_WIDTH), mThumbSize) / 2.0f;
            this.mThumbTouchRadius = mThumbRadius;
            this.mCustomDrawableMaxHeight = Math.max(mThumbTouchRadius, mTickRadius) * 2.0f;
            initThumbBitmap();
        }
        requestLayout();
        invalidate();
    }

    /**
     * call this will do not draw thumb, true if hide.
     */
    public void hideThumb(boolean hide) {
        mHideThumb = hide;
        invalidate();
    }

    /**
     * call this will do not draw the text which below thumb. true if hide.
     */
    public void hideThumbText(boolean hide) {
        mShowThumbText = !hide;
        invalidate();
    }

    /**
     * set the seek bar's thumb's color.
     *
     * @param thumbColor colorInt
     */
    public void thumbColor(@ColorInt int thumbColor) {
        this.mThumbColor = thumbColor;
        this.mPressedThumbColor = thumbColor;
        invalidate();
    }

    public void thumbColorStateList(@NonNull ColorStateList thumbColorStateList) {
        initThumbColor(thumbColorStateList, mThumbColor);
        invalidate();
    }

    public void setTickMarksDrawable(Drawable drawable) {
        if (drawable == null) {
            this.mTickMarksDrawable = null;
            this.mUnselectTickMarksBitmap = null;
            this.mSelectTickMarksBitmap = null;
        } else {
            this.mTickMarksDrawable = drawable;
            this.mTickRadius = Math.min(ScreenUtils.dp2px(mContext, THUMB_MAX_WIDTH), mTickMarksSize) / 2.0f;
            this.mCustomDrawableMaxHeight = Math.max(mThumbTouchRadius, mTickRadius) * 2.0f;
            initTickMarksBitmap();
        }
        invalidate();
    }

    public void tickMarksColor(@ColorInt int tickMarksColor) {
        this.mSelectedTickMarksColor = tickMarksColor;
        this.mUnSelectedTickMarksColor = tickMarksColor;
        invalidate();
    }

    public void tickMarksColor(@NonNull ColorStateList tickMarksColorStateList) {
        initTickMarksColor(tickMarksColorStateList, mSelectedTickMarksColor);
        invalidate();
    }

    public void tickTextsColor(@ColorInt int tickTextsColor) {
        mUnselectedTextsColor = tickTextsColor;
        mSelectedTextsColor = tickTextsColor;
        mHoveredTextColor = tickTextsColor;
        invalidate();
    }

    public void tickTextsColorStateList(@NonNull ColorStateList tickTextsColorStateList) {
        initTickTextsColor(tickTextsColorStateList, mSelectedTextsColor);
        invalidate();
    }

    public void setDecimalScale(int scale) {
        this.mScale = scale;
    }

    public void setIndicatorTextFormat(String format) {
        this.mIndicatorTextFormat = format;
        initTextsArray();
        updateStayIndicator();
    }

    public void customSectionTrackColor(@NonNull ColorCollector collector) {
        int[] colorArray = new int[mTicksCount - 1 > 0 ? mTicksCount - 1 : 1];
        for (int i = 0; i < colorArray.length; i++) {
            //set the default section color
            colorArray[i] = mBackgroundTrackColor;
        }
        this.mCustomTrackSectionColorResult = collector.collectSectionTrackColor(colorArray);
        this.mSectionTrackColorArray = colorArray;
        invalidate();
    }

    public void customTickTexts(@NonNull String[] tickTextsArr) {
        this.mTickTextsCustomArray = tickTextsArr;
        if (mTickTextsArr != null) {
            for (int i = 0; i < mTickTextsArr.length; i++) {
                String tickText;
                if (i < tickTextsArr.length) {
                    tickText = String.valueOf(tickTextsArr[i]);
                } else {
                    tickText = "";
                }
                int index = i;
                if (mR2L) {
                    index = mTicksCount - 1 - i;
                }
                mTickTextsArr[index] = tickText;
                if (mTextPaint != null && mRect != null) {
                    mTextPaint.getTextBounds(tickText, 0, tickText.length(), mRect);
                    mTickTextsWidth[index] = mRect.width();
                }
            }
            invalidate();
        }
    }

    /**
     * Set the custom tick texts' typeface you want.
     *
     * @param typeface The typeface for tickTexts.
     */
    public void customTickTextsTypeface(@NonNull Typeface typeface) {
        this.mTextsTypeface = typeface;
        measureTickTextsBonds();
        requestLayout();
        invalidate();
    }

    /**
     * Set the listener to listen the seeking params changing.
     *
     * @param listener OnSeekChangeListener
     */
    public void setOnSeekChangeListener(@NonNull OnSeekChangeListener listener) {
        this.mSeekChangeListener = listener;
    }

    /**
     * only show the tick texts on both of ends seek bar, make sure you hava called the attr:show tick text before.
     *
     * @param onlyShow true if only show the tick texts on both of ends seek bar
     */
    public void showBothEndsTickTextsOnly(boolean onlyShow) {
        this.mShowBothTickTextsOnly = onlyShow;
    }

    /**
     * prevent user from seeking
     *
     * @param seekAble true if user can seek
     */
    public void setUserSeekAble(boolean seekAble) {
        this.mUserSeekable = seekAble;
    }

    /**
     * Sets the tick count
     *
     * @param tickCount
     */
    public synchronized void setTickCount(int tickCount) {
        if (mTicksCount < 0 || mTicksCount > 50) {
            throw new IllegalArgumentException(
                    "the Argument: TICK COUNT must be limited between (0-50), Now is " + mTicksCount);
        }
        mTicksCount = tickCount;
        collectTicksInfo();
        initTextsArray();
        initSeekBarInfo();
        refreshSeekBarLocation();
        invalidate();
        updateStayIndicator();
    }

    /**
     * Sets the thumb move to the closed tick after touched up automatically, default true
     *
     * @param adjustAuto true if auto move after touched up.
     */
    public void setThumbAdjustAuto(boolean adjustAuto) {
        mAdjustAuto = adjustAuto;
    }


    public void setProgressTrackColor(int progressTrackColor) {
        this.mProgressTrackColor = progressTrackColor;
        if (mStockPaint != null) {
            mStockPaint.setColor(mProgressTrackColor);
            invalidate();
        }
    }

    public void setIndicatorColor(int indicatorColor) {
        this.mIndicatorColor = indicatorColor;
        this.mIndicator = null;
        this.initIndicatorContentView();
        invalidate();
    }

    /*------------------API END-------------------*/


}