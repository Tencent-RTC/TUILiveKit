package com.tencent.effect.beautykit.view.widget.indicatorseekbar;

import android.content.Context;
import android.content.res.ColorStateList;
import android.graphics.Color;
import android.graphics.Typeface;
import android.graphics.drawable.Drawable;

import android.view.View;

import com.tencent.effect.beautykit.utils.ScreenUtils;




public class IndicatorSeekBarParams {
    final Context context;
    //seek bar
    float max = 100;
    float min = 0;
    float progress = 0;
    boolean progressValueFloat = false;
    boolean seekSmoothly = false;
    boolean r2l = false;
    boolean userSeekable = true;
    boolean onlyThumbDraggable = false;
    boolean clearPadding = false;
    //indicator
    int showIndicatorType = IndicatorType.ROUNDED_RECTANGLE;
    int indicatorColor = Color.parseColor("#FF4081");
    int indicatorTextColor = Color.parseColor("#FFFFFF");
    int indicatorTextSize = 0;
    View indicatorContentView = null;
    View indicatorTopContentView = null;
    //track
    int trackBackgroundSize = 0;
    int trackBackgroundColor = Color.parseColor("#D7D7D7");
    int trackProgressSize = 0;
    int trackProgressColor = Color.parseColor("#FF4081");
    boolean trackRoundedCorners = false;
    //thumbText
    int thumbTextColor = Color.parseColor("#FF4081");
    boolean showThumbText = false;
    //thumb
    int thumbSize = 0;
    int thumbColor = Color.parseColor("#FF4081");
    ColorStateList thumbColorStateList = null;
    Drawable thumbDrawable = null;
    //tickTexts
    boolean showTickText;
    int tickTextsColor = Color.parseColor("#FF4081");
    int tickTextsSize = 0;
    String[] tickTextsCustomArray = null;
    Typeface tickTextsTypeFace = Typeface.DEFAULT;
    ColorStateList tickTextsColorStateList = null;
    //tickMarks
    int tickCount = 0;
    int showTickMarksType = TickMarkType.NONE;
    int tickMarksColor = Color.parseColor("#FF4081");
    int tickMarksSize = 0;
    Drawable tickMarksDrawable = null;
    boolean tickMarksEndsHide = false;
    boolean tickMarksSweptHide = false;
    ColorStateList tickMarksColorStateList = null;

    IndicatorSeekBarParams(Context context) {
        this.context = context;
        this.indicatorTextSize = ScreenUtils.sp2px(context, 14);
        this.trackBackgroundSize = ScreenUtils.dp2px(context, 2);
        this.trackProgressSize = ScreenUtils.dp2px(context, 2);
        this.tickMarksSize = ScreenUtils.dp2px(context, 10);
        this.tickTextsSize = ScreenUtils.sp2px(context, 13);
        this.thumbSize = ScreenUtils.dp2px(context, 14);
    }



}