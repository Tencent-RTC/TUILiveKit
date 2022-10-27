/*
 * Copyright (C) 2015 tyrantgit
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.tencent.qcloud.tuikit.tuigift.view.like;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.RelativeLayout;

import com.tencent.qcloud.tuikit.tuigift.R;

import java.util.Random;

/**
 * Module:   TUIGiftHeartLayout
 * <p>
 * Function: 飘心动画界面布局类
 * </p>
 * 通过动画控制每个心形界面的显示
 * TUIGiftPathAnimator 控制显示路径
 * TUIGiftHeartView 单个心形界面
 */
public class TUIGiftHeartLayout extends RelativeLayout {

    private TUIGiftAbstractPathAnimator mAnimator;

    private int mDefStyleAttr = 0;
    private int mTextHeight;
    private int mBitmapHeight;
    private int mBitmapWidth;
    private int mInitX;
    private int mPointx;

    public TUIGiftHeartLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        findViewById(context);
        initHeartDrawable();
        init(attrs, mDefStyleAttr);
    }

    private void findViewById(Context context) {
        LayoutInflater.from(context).inflate(R.layout.tuigift_heart_layout, this);
        Bitmap bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.tuigift_ic_like_png);
        mBitmapHeight = bitmap.getWidth();
        mBitmapWidth = bitmap.getHeight();
        mTextHeight = sp2px(getContext(), 20) + mBitmapHeight / 2;

        mPointx = mBitmapWidth;//随机上浮方向的x坐标

        bitmap.recycle();
    }

    private static int sp2px(Context context, float spValue) {
        final float fontScale = context.getResources().getDisplayMetrics().scaledDensity;
        return (int) (spValue * fontScale + 0.5f);
    }


    private void init(AttributeSet attrs, int defStyleAttr) {
        final TypedArray a = getContext().obtainStyledAttributes(
                attrs, R.styleable.TUIGiftHeartLayout, defStyleAttr, 0);

        mInitX = 30;
        if (mPointx <= mInitX && mPointx >= 0) {
            mPointx -= 10;
        } else if (mPointx >= -mInitX && mPointx <= 0) {
            mPointx += 10;
        } else {
            mPointx = mInitX;
        }

        mAnimator = new TUIGiftPathAnimator(TUIGiftAbstractPathAnimator.Config.fromTypeArray(a, mInitX, mTextHeight,
                mPointx, mBitmapWidth, mBitmapHeight));
        a.recycle();
    }

    public void clearAnimation() {
        for (int i = 0; i < getChildCount(); i++) {
            getChildAt(i).clearAnimation();
        }
        removeAllViews();
    }

    private void resourceLoad() {
        mHearts = new Bitmap[drawableIds.length];
        mHeartsDrawable = new BitmapDrawable[drawableIds.length];
        for (int i = 0; i < drawableIds.length; i++) {
            mHearts[i] = BitmapFactory.decodeResource(getResources(), drawableIds[i]);
            mHeartsDrawable[i] = new BitmapDrawable(getResources(), mHearts[i]);
        }
    }

    private static int[]            drawableIds = new int[]{
            R.drawable.tuigift_heart0, R.drawable.tuigift_heart1, R.drawable.tuigift_heart2, R.drawable.tuigift_heart3,
            R.drawable.tuigift_heart4, R.drawable.tuigift_heart5, R.drawable.tuigift_heart6, R.drawable.tuigift_heart7,
            R.drawable.tuigift_heart8,};
    private        Random           mRandom     = new Random();
    private static Drawable[]       sDrawables;
    private        Bitmap[]         mHearts;
    private        BitmapDrawable[] mHeartsDrawable;

    private void initHeartDrawable() {
        int size = drawableIds.length;
        sDrawables = new Drawable[size];
        for (int i = 0; i < size; i++) {
            sDrawables[i] = getResources().getDrawable(drawableIds[i]);
        }
        resourceLoad();
    }

    public void addFavor() {
        TUIGiftHeartView heartView = new TUIGiftHeartView(getContext());
        heartView.setDrawable(mHeartsDrawable[mRandom.nextInt(8)]);
        mAnimator.start(heartView, this);
    }

}
