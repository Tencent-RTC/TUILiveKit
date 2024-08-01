package com.tencent.effect.beautykit.utils;

import android.content.Context;
import android.content.res.ColorStateList;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.LayerDrawable;

import androidx.annotation.ColorInt;

public class CustomDrawableUtils {


    public static LayerDrawable createSeekBarThumbDrawable(Context context, @ColorInt int color) {
        GradientDrawable backgroundDrawable = new GradientDrawable();
        backgroundDrawable.setShape(GradientDrawable.OVAL);
        backgroundDrawable.setSize(ScreenUtils.dp2px(context, 15), ScreenUtils.dp2px(context, 15));
        backgroundDrawable.setColor(0xFFFFFFFF);

        GradientDrawable progressDrawable = new GradientDrawable();
        progressDrawable.setShape(GradientDrawable.OVAL);
        progressDrawable.setColor(color);

        LayerDrawable layerDrawable =
                new LayerDrawable(new GradientDrawable[]{backgroundDrawable, progressDrawable});
        layerDrawable.setId(0, android.R.id.background);
        layerDrawable.setId(1, android.R.id.progress);
        layerDrawable.setLayerInset(1, ScreenUtils.dp2px(context, 3), ScreenUtils.dp2px(context, 3),
                ScreenUtils.dp2px(context, 3), ScreenUtils.dp2px(context, 3));
        return layerDrawable;
    }


    public static ColorStateList createRadioGroupColorStateList(@ColorInt int checkedColor, @ColorInt int normalColor) {
        int[][] states = new int[][]{
                new int[]{android.R.attr.state_checked},
                new int[]{0},
        };
        int[] colors = new int[]{
                checkedColor,
                normalColor
        };
        return new ColorStateList(states, colors);
    }
}
