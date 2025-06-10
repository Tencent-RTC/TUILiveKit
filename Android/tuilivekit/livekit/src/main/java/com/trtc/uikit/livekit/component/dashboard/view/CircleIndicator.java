package com.trtc.uikit.livekit.component.dashboard.view;

import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.util.AttributeSet;
import android.view.View;
import android.widget.LinearLayout;

import androidx.annotation.Nullable;

import com.trtc.tuikit.common.util.ScreenUtil;
import com.trtc.uikit.livekit.R;

public class CircleIndicator extends LinearLayout {

    private int mNormalColor   = Color.TRANSPARENT;
    private int mSelectedColor = Color.TRANSPARENT;
    private int mCircleRadius  = 0;
    private int mCircleCount   = 0;
    private int mSelected      = -1;

    public CircleIndicator(Context context) {
        this(context, null);
    }

    public CircleIndicator(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        setOrientation(HORIZONTAL);
        setNormalColor(context.getResources().getColor(R.color.common_text_color_disabled));
        setSelectedColor(context.getResources().getColor(R.color.common_text_color_primary));
        setCircleCount(0);
        setSelected(-1);
    }

    public void setCircleCount(int count) {
        mCircleCount = count;
    }

    public void setCircleRadius(int radius) {
        mCircleRadius = radius;
    }

    public void setSelected(int index) {
        mSelected = index;
        update();
    }

    public void setNormalColor(int color) {
        mNormalColor = color;
    }

    public void setSelectedColor(int color) {
        mSelectedColor = color;
    }

    private void update() {
        if (getChildCount() != mCircleCount) {
            removeAllViews();
            for (int i = 0; i < mCircleCount; i++) {
                View circleView = createCircleView();
                LinearLayout.LayoutParams params = new LayoutParams(2 * mCircleRadius, 2 * mCircleRadius);
                params.leftMargin = ScreenUtil.dip2px(5);
                params.rightMargin = ScreenUtil.dip2px(5);
                addView(circleView, params);
            }
        }
        for (int i = 0; i < getChildCount(); i++) {
            View circleView = getChildAt(i);
            Drawable drawable = circleView.getBackground();
            if (drawable instanceof GradientDrawable) {
                int color = i == mSelected ? mSelectedColor : mNormalColor;
                GradientDrawable gradientDrawable = (GradientDrawable) drawable;
                gradientDrawable.setColor(color);
                circleView.setBackground(gradientDrawable);
            }
        }
    }

    private View createCircleView() {
        View view = new View(getContext());
        GradientDrawable drawable = createRoundGradientDrawable(mNormalColor);
        view.setBackground(drawable);
        return view;
    }

    private GradientDrawable createRoundGradientDrawable(int color) {
        GradientDrawable gradientDrawable = new GradientDrawable();
        gradientDrawable.setColor(color);
        gradientDrawable.setCornerRadius(mCircleRadius);
        return gradientDrawable;
    }
}
