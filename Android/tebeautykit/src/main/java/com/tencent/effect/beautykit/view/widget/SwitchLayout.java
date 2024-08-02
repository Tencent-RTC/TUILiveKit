package com.tencent.effect.beautykit.view.widget;

import android.content.Context;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.ColorRes;
import androidx.annotation.DrawableRes;
import androidx.annotation.Nullable;

import com.tencent.effect.beautykit.R;


public class SwitchLayout extends LinearLayout {

    private @DrawableRes int bgDrawable = R.drawable.te_beauty_panel_switch_layout_bg;
    private @DrawableRes int checkedDrawable = R.drawable.te_beauty_panel_switch_layout_checked_bg;
    private @ColorRes int textCheckedColor = R.color.te_beauty_color_CC000000;
    private @ColorRes int textCommonColor = R.color.te_beauty_color_4D000000;


    private SwitchLayoutListener switchLayoutListener;
    private TextView leftTextView;
    private TextView rightTextView;
    public static final int SWITCH_LEFT_CHECKED = 1;
    public static final int SWITCH_RIGHT_CHECKED = 2;
    private int currentCheckedItem = SWITCH_LEFT_CHECKED;

    private int textSize = 12;
    private int defaultMargin = 2;

    public SwitchLayout(Context context) {
        this(context, null);
    }

    public SwitchLayout(Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SwitchLayout(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.initViews(context);
    }

    private void initViews(Context context) {

        setOrientation(HORIZONTAL);
        this.setBackgroundResource(this.bgDrawable);
        this.setGravity(Gravity.CENTER_VERTICAL);
        this.leftTextView = new TextView(context);
        this.leftTextView.setTextSize(textSize);
        this.leftTextView.setTextColor(getResources().getColor(textCommonColor));
        this.leftTextView.setOnClickListener(this::onItemClick);
        this.leftTextView.setGravity(Gravity.CENTER);


        LayoutParams leftLayoutParams = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT);
        leftLayoutParams.weight = 1;
        leftLayoutParams.topMargin = defaultMargin;
        leftLayoutParams.bottomMargin = defaultMargin;
        leftLayoutParams.leftMargin = defaultMargin;
        this.addView(leftTextView, leftLayoutParams);


        rightTextView = new TextView(context);
        this.rightTextView.setTextSize(textSize);
        this.rightTextView.setTextColor(getResources().getColor(textCommonColor));
        this.rightTextView.setOnClickListener(this::onItemClick);
        this.rightTextView.setGravity(Gravity.CENTER);
        LayoutParams rightLayoutParams = new
                LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
        rightLayoutParams.weight = 1;
        rightLayoutParams.topMargin = defaultMargin;
        rightLayoutParams.bottomMargin = defaultMargin;
        rightLayoutParams.rightMargin = defaultMargin;
        this.addView(rightTextView, rightLayoutParams);
    }

    public void setText(String left, String right) {
        this.leftTextView.setText(left);
        this.rightTextView.setText(right);
    }


    public void check(int checkedItem) {
        if (SWITCH_LEFT_CHECKED == checkedItem) {
            this.leftTextView.callOnClick();
        } else if (SWITCH_RIGHT_CHECKED == checkedItem) {
            this.rightTextView.callOnClick();
        }
        this.currentCheckedItem = checkedItem;
    }

    private void onItemClick(View view) {
        if (view == this.leftTextView) {
            this.leftTextView.setBackgroundResource(this.checkedDrawable);
            this.rightTextView.setBackground(null);
            this.leftTextView.setTextColor(getResources().getColor(textCheckedColor));
            this.rightTextView.setTextColor(getResources().getColor(textCommonColor));
            this.currentCheckedItem = SWITCH_LEFT_CHECKED;
        } else {
            this.rightTextView.setBackgroundResource(this.checkedDrawable);
            this.leftTextView.setBackground(null);
            this.leftTextView.setTextColor(getResources().getColor(textCommonColor));
            this.rightTextView.setTextColor(getResources().getColor(textCheckedColor));
            this.currentCheckedItem = SWITCH_RIGHT_CHECKED;
        }
        if (switchLayoutListener != null) {
            this.switchLayoutListener.onSwitchChange(this.currentCheckedItem);
        }
    }

    public void setSwitchLayoutListener(SwitchLayoutListener switchLayoutListener) {
        this.switchLayoutListener = switchLayoutListener;
    }

    public int getCurrentCheckedItem() {
        return currentCheckedItem;
    }



    public void setColor(@DrawableRes int bgDrawable, @DrawableRes int checkedDrawable, @ColorRes int textCheckedColor,
                         @ColorRes int textCommonColor) {
        this.bgDrawable = bgDrawable;
        this.checkedDrawable = checkedDrawable;
        this.textCheckedColor = textCheckedColor;
        this.textCommonColor = textCommonColor;
        this.setBackgroundResource(this.bgDrawable);
        if (currentCheckedItem == SWITCH_LEFT_CHECKED) {
            this.leftTextView.setBackgroundResource(this.checkedDrawable);
            this.leftTextView.setTextColor(getResources().getColor(textCheckedColor));
            this.rightTextView.setTextColor(getResources().getColor(textCommonColor));
        } else {
            this.rightTextView.setBackgroundResource(this.checkedDrawable);
            this.leftTextView.setTextColor(getResources().getColor(textCommonColor));
            this.rightTextView.setTextColor(getResources().getColor(textCheckedColor));
        }
    }


    public void setTextSize(int textSize) {
        this.textSize = textSize;
        this.leftTextView.setTextSize(textSize);
        this.leftTextView.setTextSize(textSize);
    }


    public interface SwitchLayoutListener {
        void onSwitchChange(int text);
    }

}
