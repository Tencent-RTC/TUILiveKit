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

package com.trtc.uikit.livekit.common.uicomponent.gift.view.like;

import android.content.res.Resources;
import android.content.res.TypedArray;
import android.graphics.Path;
import android.view.View;
import android.view.ViewGroup;

import com.trtc.uikit.livekit.R;

import java.security.SecureRandom;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Module:   GiftAbstractPathAnimator
 * Function: Heart-fluttering animation base class
 */
public abstract class GiftAbstractPathAnimator {
    private final SecureRandom mRandom;
    protected final Config     mConfig;

    public GiftAbstractPathAnimator(Config config) {
        mConfig = config;
        mRandom = new SecureRandom();
    }

    public float randomRotation() {
        return mRandom.nextFloat() * 28.6F - 14.3F;
    }

    public Path createPath(AtomicInteger counter, View view, int factor) {
        Random r = mRandom;
        int x = r.nextInt(mConfig.xRand);
        int x2 = r.nextInt(mConfig.xRand);
        final int y = view.getHeight() - mConfig.initY;
        int y2 = counter.intValue() * 15 + mConfig.animLength * factor + r.nextInt(mConfig.animLengthRand);
        factor = y2 / mConfig.bezierFactor;
        x = mConfig.xPointFactor + x;
        x2 = mConfig.xPointFactor + x2;
        final int y3 = y - y2;
        y2 = y - y2 / 2;
        Path p = new Path();
        p.moveTo(mConfig.initX, y);
        p.cubicTo(mConfig.initX, y - factor, x, y2 + factor, x, y2);
        p.moveTo(x, y2);
        p.cubicTo(x, y2 - factor, x2, y3 + factor, x2, y3);
        return p;
    }

    public abstract void start(View child, ViewGroup parent);

    public static class Config {
        public int initX;
        public int initY;
        public int xRand;
        public int animLengthRand;
        public int bezierFactor;
        public int xPointFactor;
        public int animLength;
        public int heartWidth;
        public int heartHeight;
        public int animDuration;

        public static Config fromTypeArray(TypedArray typedArray, float x, float y, int pointX,
                                           int heartWidth, int heartHeight) {
            Config config = new Config();
            Resources res = typedArray.getResources();
            config.initX = (int) typedArray.getDimension(R.styleable.LiveKitGiftHeartLayout_initX,
                    x);
            config.initY = (int) typedArray.getDimension(R.styleable.LiveKitGiftHeartLayout_initY,
                    y);
            config.xRand = (int) typedArray.getDimension(R.styleable.LiveKitGiftHeartLayout_xRand,
                    res.getDimensionPixelOffset(R.dimen.livekit_gift_heart_anim_bezier_x_rand));
            config.animLength = (int) typedArray.getDimension(R.styleable.LiveKitGiftHeartLayout_animLength,
                    res.getDimensionPixelOffset(R.dimen.livekit_gift_heart_anim_length));
            config.animLengthRand = (int) typedArray.getDimension(R.styleable.LiveKitGiftHeartLayout_animLengthRand,
                    res.getDimensionPixelOffset(R.dimen.livekit_gift_heart_anim_length_rand));
            config.bezierFactor = typedArray.getInteger(R.styleable.LiveKitGiftHeartLayout_bezierFactor,
                    res.getInteger(R.integer.livekit_gift_heart_anim_bezier_factor));
            config.xPointFactor = pointX;
            config.heartWidth = heartWidth;
            config.heartHeight = heartHeight;
            config.animDuration = typedArray.getInteger(R.styleable.LiveKitGiftHeartLayout_anim_duration,
                    res.getInteger(R.integer.livekit_gift_heart_anim_duration));
            return config;
        }


    }


}

