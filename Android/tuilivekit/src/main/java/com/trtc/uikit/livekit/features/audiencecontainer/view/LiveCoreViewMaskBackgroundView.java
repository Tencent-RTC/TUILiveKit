package com.trtc.uikit.livekit.features.audiencecontainer.view;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.util.AttributeSet;

import androidx.annotation.ColorInt;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.content.res.AppCompatResources;
import androidx.appcompat.widget.AppCompatImageView;
import androidx.lifecycle.Observer;

import com.bumptech.glide.Glide;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.request.target.SimpleTarget;
import com.bumptech.glide.request.transition.Transition;
import com.google.gson.Gson;
import com.tencent.cloud.tuikit.engine.extension.TUILiveLayoutManager.SeatLayout;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.imageloader.BlurUtils;
import com.trtc.tuikit.common.util.ScreenUtil;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.AudienceManager;

public class LiveCoreViewMaskBackgroundView extends AppCompatImageView {
    private final        int           OID         = hashCode();
    private static final int           BLUR_RADIUS = 20;
    private static final LiveKitLogger LOGGER      = LiveKitLogger.getFeaturesLogger("LiveCoreViewMaskBackgroundView");

    private       AudienceManager      mManager;
    private       int                  mParentWidth        = ScreenUtil.dip2px(720);
    private       int                  mParentHeight       = ScreenUtil.dip2px(1080);
    private       Drawable             mBackgroundDrawable;
    private       int                  mBackgroundColor    = Color.TRANSPARENT;
    private       Rect                 mTopRect            = new Rect();
    private       Rect                 mMiddleRect         = new Rect();
    private       Rect                 mBottomRect         = new Rect();
    private       SeatLayout           mSeatLayout         = null;
    private       boolean              mEnableBlur         = true;
    private final Observer<SeatLayout> mSeatLayoutObserver = this::onSeatLayoutChange;

    public LiveCoreViewMaskBackgroundView(@NonNull Context context) {
        super(context);
    }

    public LiveCoreViewMaskBackgroundView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public LiveCoreViewMaskBackgroundView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mManager.getState().roomState.seatLayout.observeForever(mSeatLayoutObserver);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        mManager.getState().roomState.seatLayout.removeObserver(mSeatLayoutObserver);
    }

    public void init(AudienceManager manager) {
        mManager = manager;
    }

    @Override
    public void setImageDrawable(@Nullable Drawable drawable) {
        mBackgroundDrawable = drawable;
        if (mBackgroundDrawable != null) {
            mBackgroundDrawable.setCallback(this);
        }
        invalidate();
    }

    @Override
    public void setImageResource(int resId) {
        mBackgroundDrawable = AppCompatResources.getDrawable(getContext(), resId);
        if (mBackgroundDrawable != null) {
            mBackgroundDrawable.setCallback(this);
        }
        invalidate();
    }

    @Override
    public void setBackground(Drawable background) {
        mBackgroundDrawable = background;
        if (mBackgroundDrawable != null) {
            mBackgroundDrawable.setCallback(this);
        }
        invalidate();
    }

    @Override
    public void setBackgroundResource(int resId) {
        mBackgroundDrawable = AppCompatResources.getDrawable(getContext(), resId);
        if (mBackgroundDrawable != null) {
            mBackgroundDrawable.setCallback(this);
        }
        invalidate();
    }

    public void enableBlur(boolean enable) {
        mEnableBlur = enable;
    }

    @Override
    public void setBackgroundColor(@ColorInt int color) {
        mBackgroundColor = color;
        invalidate();
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);
        LOGGER.info("onSizeChanged OID:" + OID + " w:" + w + " h:" + h + " oldw:" + oldw + " oldh:" + oldh);
        if (w != oldw || h != oldh) {
            onSeatLayoutChange(mSeatLayout);
        }
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        mParentWidth = getMeasuredWidth();
        mParentHeight = getMeasuredHeight();
        LOGGER.info("onMeasure OID:" + OID + " mParentWidth:" + mParentWidth + " mParentHeight:" + mParentHeight);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        boolean isFullScreen = isFullScreenLayoutBySeatLayout(mSeatLayout);
        LOGGER.info("onDraw OID:" + OID + ",: isFullScreen:" + isFullScreen
                + ", roomId:" + mManager.getRoomState().liveInfo.roomId
                + ", mTopRect:" + mTopRect.toString() + ", " + "mMiddleRect:" + mMiddleRect.toString()
                + ", mBottomRect:" + mBottomRect.toString() + ",mBackgroundColor:" + mBackgroundColor
                + ", mBackgroundDrawable:" + mBackgroundDrawable);
        if (isFullScreen) {
            drawableTransparent(canvas);
            return;
        }
        if (mBackgroundDrawable == null) {
            if (mBackgroundColor == Color.TRANSPARENT) {
                drawableTransparent(canvas);
            } else {
                drawableColor(canvas, mBackgroundColor);
            }
        } else {
            drawDrawable(canvas, mBackgroundDrawable);
        }
    }

    private void drawableTransparent(Canvas canvas) {
        Paint paint = new Paint();
        paint.setColor(Color.TRANSPARENT);
        Rect parentRect = new Rect(0, 0, mParentWidth, mParentHeight);
        canvas.drawRect(parentRect, paint);
    }

    private void drawableColor(Canvas canvas, int mBackgroundColor) {
        boolean isFullScreen = isFullScreenLayoutBySeatLayout(mSeatLayout);
        if (isFullScreen) {
            drawableTransparent(canvas);
            return;
        }
        Paint paint = new Paint();
        paint.setColor(mBackgroundColor);
        canvas.drawRect(mTopRect, paint);

        paint.setColor(Color.TRANSPARENT);
        canvas.drawRect(mMiddleRect, paint);

        paint.setColor(mBackgroundColor);
        canvas.drawRect(mBottomRect, paint);
    }

    private void drawDrawable(Canvas canvas, Drawable drawable) {
        boolean isFullScreen = isFullScreenLayoutBySeatLayout(mSeatLayout);
        if (isFullScreen) {
            drawableTransparent(canvas);
            return;
        }
        Bitmap bitmap = drawableToBitmap(drawable);
        if (bitmap == null) {
            LOGGER.error("drawDrawable bitmap is null");
            return;
        }
        float topScale = mTopRect.bottom / (float) mParentHeight;
        Rect bitmapTopRect = new Rect(0, 0, bitmap.getWidth(), (int) (bitmap.getHeight() * topScale));
        canvas.drawBitmap(bitmap, bitmapTopRect, mTopRect, null);

        Paint paint = new Paint();
        paint.setColor(Color.TRANSPARENT);
        canvas.drawRect(mMiddleRect, paint);

        float bottomScale = mBottomRect.top / (float) mParentHeight;
        Rect bitmapBottomRect = new Rect(0, (int) (bitmap.getHeight() * bottomScale), bitmap.getWidth(),
                bitmap.getHeight());
        canvas.drawBitmap(bitmap, bitmapBottomRect, mBottomRect, null);
    }

    public Bitmap drawableToBitmap(Drawable drawable) {
        if (drawable instanceof BitmapDrawable) {
            Bitmap bitmap = ((BitmapDrawable) drawable).getBitmap();
            return mEnableBlur ? BlurUtils.blur(getContext(), bitmap, BLUR_RADIUS) : bitmap;
        }
        int width = drawable.getIntrinsicWidth();
        int height = drawable.getIntrinsicHeight();

        if (width <= 0 || height <= 0) {
            LOGGER.error("Drawable dimensions are invalid");
            return null;
        }
        Bitmap bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, canvas.getWidth(), canvas.getHeight());
        drawable.draw(canvas);
        return mEnableBlur ? BlurUtils.blur(getContext(), bitmap, BLUR_RADIUS) : bitmap;
    }

    public void setBackgroundUrl(String url) {
        LOGGER.info("setBackgroundUrl OID:" + OID + ", roomId:" + mManager.getRoomState().liveInfo.roomId
                + ", mTopRect:" + mTopRect.toString() + ", " + "mMiddleRect:" + mMiddleRect.toString()
                + ", mBottomRect:" + mBottomRect.toString() + ",mBackgroundColor:" + mBackgroundColor
                + ", mBackgroundDrawable:" + mBackgroundDrawable);
        if (TextUtils.isEmpty(url)) {
            return;
        }
        int color;
        try {
            if (url.startsWith("0x") || url.startsWith("0X")) {
                url = url.substring(2);
            } else if (url.startsWith("#")) {
                url = url.substring(1);
            }
            color = Integer.parseUnsignedInt(url, 16);
        } catch (Exception e) {
            LOGGER.info("Exception OID:" + OID + ", url:" + url);
            mBackgroundColor = Color.TRANSPARENT;
            Glide.with(getContext()).load(url).diskCacheStrategy(DiskCacheStrategy.ALL).into(new SimpleTarget<Drawable>() {
                @Override
                public void onResourceReady(@NonNull Drawable resource, Transition<? super Drawable> transition) {
                    LOGGER.info("onBackgroundUrlChange OID:" + OID + ", roomId:" + mManager.getRoomState().liveInfo.roomId+ ", resource:" + resource);
                    LiveCoreViewMaskBackgroundView.this.setImageDrawable(resource);
                }

                @Override
                public void onLoadFailed(@Nullable Drawable errorDrawable) {
                    LOGGER.info("onLoadFailed OID:" + OID + ", roomId:" + mManager.getRoomState().liveInfo.roomId);
                    mBackgroundDrawable = null;
                    setBackgroundColor(Color.TRANSPARENT);
                }
            });
            return;
        }
        mBackgroundDrawable = null;
        setBackgroundColor(color);
    }

    public boolean isFullScreenLayoutBySeatLayout(SeatLayout seatLayout) {
        if (seatLayout == null || seatLayout.seatList == null) {
            return true;
        }
        if (seatLayout.seatList.size() <= 1) {
            return true;
        }
        for (TUIRoomDefine.SeatFullInfo region : seatLayout.seatList) {
            if (region.width == seatLayout.canvasWidth && region.height == seatLayout.canvasHeight) {
                return true;
            }
        }
        return false;
    }

    private void onSeatLayoutChange(SeatLayout seatLayout) {
        LOGGER.info("setSeatLayout OID:" + OID + ",seatLayout:" + new Gson().toJson(seatLayout));
        if (seatLayout == null || seatLayout.seatList == null) {
            invalidate();
            return;
        }
        mSeatLayout = seatLayout;
        Rect mRect = new Rect();
        mRect.left = Integer.MAX_VALUE;
        mRect.top = Integer.MAX_VALUE;
        mRect.right = 0;
        mRect.bottom = 0;
        for (TUIRoomDefine.SeatFullInfo region : seatLayout.seatList) {
            mRect.left = Math.min(mRect.left, region.x);
            mRect.top = Math.min(mRect.top, region.y);
            mRect.right = Math.max(mRect.right, region.x + region.width);
            mRect.bottom = Math.max(mRect.bottom, region.y + region.height);
        }
        float scale = mParentWidth / (float) seatLayout.canvasWidth;
        int width = (int) (mRect.width() * scale);
        int height = (int) (mRect.height() * scale);
        int top = (int) (mRect.top * scale);
        int left = (int) (mRect.left * scale);
        LOGGER.info("addFillScreenView OID:" + OID + " l:" + left + ",t:" + top + ",w:" + width + ",h:" + height);

        mTopRect = new Rect(0, 0, width, top);
        mMiddleRect = new Rect(0, top, width, top + height);
        mBottomRect = new Rect(0, top + height, width, mParentHeight);

        invalidate();
    }
}


