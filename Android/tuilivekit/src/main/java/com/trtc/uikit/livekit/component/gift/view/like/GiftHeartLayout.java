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

package com.trtc.uikit.livekit.component.gift.view.like;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.RelativeLayout;

import com.trtc.uikit.livekit.R;

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.List;

/**
 * Module:   GiftHeartLayout
 * <p>
 * Function: Drifting Animation Interface Layout Class
 * </p>
 * Control the display of each cardioid interface with animations
 * GiftPathAnimator Controls the display path
 * GiftHeartView A single cardioid interface
 */
public class GiftHeartLayout extends RelativeLayout {

    private static final int HEART_VIEW_COUNT_MAX = 30;

    private       GiftAbstractPathAnimator mAnimator;
    private final List<GiftHeartView>      mHeartViewList = new ArrayList<>();

    private int mTextHeight;
    private int mBitmapHeight;
    private int mBitmapWidth;
    private int mPointX;

    public GiftHeartLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        findViewById(context);
        resourceLoad();
        init(attrs, 0);
    }

    private void findViewById(Context context) {
        LayoutInflater.from(context).inflate(R.layout.gift_layout_heart_layout, this);
        Bitmap bitmap = BitmapFactory.decodeResource(getResources(), R.drawable.gift_ic_like_png);
        mBitmapHeight = bitmap.getWidth();
        mBitmapWidth = bitmap.getHeight();
        mTextHeight = sp2px(getContext(), 20) + mBitmapHeight / 2;

        mPointX = mBitmapWidth;//The x-coordinate of the random upward direction

        bitmap.recycle();
    }

    private static int sp2px(Context context, float spValue) {
        final float fontScale = context.getResources().getDisplayMetrics().scaledDensity;
        return (int) (spValue * fontScale + 0.5f);
    }

    private void init(AttributeSet attrs, int defStyleAttr) {
        final TypedArray a = getContext().obtainStyledAttributes(
                attrs, R.styleable.LiveKitGiftHeartLayout, defStyleAttr, 0);

        int mInitX = 30;
        if (mPointX <= mInitX && mPointX >= 0) {
            mPointX -= 10;
        } else if (mPointX >= -mInitX && mPointX <= 0) {
            mPointX += 10;
        } else {
            mPointX = mInitX;
        }

        mAnimator = new GiftPathAnimator(GiftAbstractPathAnimator.Config.fromTypeArray(a, mInitX, mTextHeight,
                mPointX, mBitmapWidth, mBitmapHeight));
        a.recycle();
    }

    public void clearAnimation() {
        for (int i = 0; i < getChildCount(); i++) {
            getChildAt(i).clearAnimation();
        }
        removeAllViews();
    }

    private void resourceLoad() {
        Bitmap[] hearts = new Bitmap[mDrawableIds.length];
        mHeartsDrawable = new BitmapDrawable[mDrawableIds.length];
        for (int i = 0; i < mDrawableIds.length; i++) {
            hearts[i] = BitmapFactory.decodeResource(getResources(), mDrawableIds[i]);
            mHeartsDrawable[i] = new BitmapDrawable(getResources(), hearts[i]);
        }
    }

    private static final int[]      mDrawableIds = new int[]{
            R.drawable.gift_heart0, R.drawable.gift_heart1, R.drawable.gift_heart2,
            R.drawable.gift_heart3, R.drawable.gift_heart4, R.drawable.gift_heart5,
            R.drawable.gift_heart6, R.drawable.gift_heart7, R.drawable.gift_heart8,};
    private final SecureRandom      mRandom = new SecureRandom();
    private        BitmapDrawable[] mHeartsDrawable;

    public void addFavor() {
        GiftHeartView heartView = null;
        for (GiftHeartView view : mHeartViewList) {
            if (view.getParent() == null) {
                heartView = view;
                break;
            }
        }
        if (heartView == null) {
            if (mHeartViewList.size() >= HEART_VIEW_COUNT_MAX) {
                return;
            }
            heartView = new GiftHeartView(getContext());
            heartView.setDrawable(mHeartsDrawable[mRandom.nextInt(8)]);
            mHeartViewList.add(heartView);
        }
        mAnimator.start(heartView, this);
    }

}
