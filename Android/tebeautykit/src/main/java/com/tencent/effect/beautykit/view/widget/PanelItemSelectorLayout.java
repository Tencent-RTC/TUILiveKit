package com.tencent.effect.beautykit.view.widget;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.PorterDuff;
import android.graphics.PorterDuffXfermode;
import android.graphics.RectF;
import android.util.AttributeSet;
import android.widget.LinearLayout;

import androidx.annotation.ColorInt;
import androidx.annotation.Nullable;

import com.tencent.effect.beautykit.R;
import com.tencent.effect.beautykit.utils.ScreenUtils;



public class PanelItemSelectorLayout extends LinearLayout {

    private Paint rectPaint;
    private Paint circlePaint;
    private PorterDuffXfermode porterDuffXfermode;

    private float cornerRadius = -1;
    private float smallRectTopMargin = -1;
    private float smallRectLeftMargin = -1;
    private float smallRectRightMargin = -1;

    private int width = -1;
    private int height = -1;


    private boolean isChecked = false;

    private RectF rect;
    private RectF rect2;

    private int checkedColor = getResources().getColor(R.color.te_beauty_color_main_color);

    public PanelItemSelectorLayout(Context context) {
        this(context, null);
    }

    public PanelItemSelectorLayout(Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public PanelItemSelectorLayout(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.init(attrs);
    }


    private void init(@Nullable AttributeSet attrs) {
        float defaultRadius = ScreenUtils.dip2px(getContext(), 10);
        float defaultMargin = ScreenUtils.dip2px(getContext(), 3);
        if (attrs != null) {
            TypedArray attributes = getContext().obtainStyledAttributes(attrs,
                    R.styleable.te_beauty_panel_item_selector);
            this.cornerRadius = attributes.getDimension(
                    R.styleable.te_beauty_panel_item_selector_corner_radius, defaultRadius);
            this.smallRectLeftMargin = attributes.getDimension(
                    R.styleable.te_beauty_panel_item_selector_rect_left_margin, defaultMargin);
            this.smallRectRightMargin = attributes.getDimension(
                    R.styleable.te_beauty_panel_item_selector_rect_right_margin, defaultMargin);
            this.smallRectTopMargin = attributes.getDimension(
                    R.styleable.te_beauty_panel_item_selector_rect_top_margin, defaultMargin);
            checkedColor = attributes.getColor(R.styleable.te_beauty_panel_item_selector_checked_color, checkedColor);
            attributes.recycle();
        } else {
            this.cornerRadius = defaultRadius;
            this.smallRectLeftMargin = defaultMargin;
            this.smallRectTopMargin = defaultMargin;
            this.smallRectRightMargin = defaultMargin;
        }

        rectPaint = new Paint();
        rectPaint.setColor(checkedColor);
        rectPaint.setAntiAlias(true);

        circlePaint = new Paint();
        circlePaint.setColor(Color.TRANSPARENT);
        circlePaint.setStyle(Paint.Style.FILL);
        circlePaint.setAntiAlias(true);
        setWillNotDraw(false);
        porterDuffXfermode = new PorterDuffXfermode(PorterDuff.Mode.CLEAR);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        if (!isChecked) {
            return;
        }
        if (width == -1 || height == -1) {
            this.width = getWidth();
            this.height = getHeight();
            float smallRoundRectWidth = width - smallRectLeftMargin - smallRectRightMargin;

            rect = new RectF(0, 0, width, height);
            rect2 = new RectF(smallRectLeftMargin, smallRectTopMargin, smallRoundRectWidth + smallRectLeftMargin,
                    smallRoundRectWidth + smallRectTopMargin);
        }

        final int saveCount = canvas.saveLayer(0, 0, width, height, null, Canvas.ALL_SAVE_FLAG);
        canvas.drawRoundRect(rect, cornerRadius, cornerRadius, rectPaint);
        circlePaint.setXfermode(porterDuffXfermode);
        canvas.drawRoundRect(rect2, cornerRadius, cornerRadius, circlePaint);
        circlePaint.setXfermode(null);
        canvas.restoreToCount(saveCount);
    }

    public void setChecked(boolean isChecked) {
        this.isChecked = isChecked;
        invalidate();
    }



    public void setCheckedColor(@ColorInt int checkedColor) {
        this.checkedColor = checkedColor;
        this.rectPaint.setColor(checkedColor);
        invalidate();
    }

    public int getCheckedColor() {
        return this.checkedColor;
    }

    public boolean isChecked() {
        return this.isChecked;
    }
}
