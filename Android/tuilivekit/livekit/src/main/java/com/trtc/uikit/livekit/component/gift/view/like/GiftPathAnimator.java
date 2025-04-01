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

import android.graphics.Matrix;
import android.graphics.Path;
import android.graphics.PathMeasure;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.LinearInterpolator;
import android.view.animation.Transformation;

import java.security.SecureRandom;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Module:   GiftPathAnimator
 * Function: Drifting path animator
 */
public class GiftPathAnimator extends GiftAbstractPathAnimator {
    private static final int MAX_PATH_COUNTS = 10;//Maximum number of paths generated

    private final Handler                mHandler;
    private       int                    mCurrentPathCounts = 0;//The number of paths that have been generated
    private final HashMap<Integer, Path> mPathVec;//The path cache that has been generated
    private final SecureRandom           mRandom;
    private final AtomicInteger          mCounter           = new AtomicInteger(0);

    public GiftPathAnimator(Config config) {
        super(config);
        mHandler = new Handler(Looper.getMainLooper());
        mPathVec = new HashMap<>();
        mRandom = new SecureRandom();
    }

    @Override
    public void start(final View child, final ViewGroup parent) {
        parent.addView(child, new ViewGroup.LayoutParams(mConfig.heartWidth, mConfig.heartHeight));

        Path path;
        ++mCurrentPathCounts;

        //If the number of paths that have already been generated exceeds the maximum setting,
        //a random path is taken from the path cache for drawing, otherwise a new path is generated
        if (mCurrentPathCounts > MAX_PATH_COUNTS) {
            path = mPathVec.get(Math.abs(mRandom.nextInt() % MAX_PATH_COUNTS) + 1);
        } else {
            path = createPath(mCounter, parent, 2);
            mPathVec.put(mCurrentPathCounts, path);
        }

        FloatAnimation anim = new FloatAnimation(path, randomRotation(), parent, child);
        anim.setDuration(mConfig.animDuration);
        anim.setInterpolator(new LinearInterpolator());
        anim.setAnimationListener(new Animation.AnimationListener() {
            @Override
            public void onAnimationEnd(Animation animation) {
                child.setAlpha(0);
                mHandler.post(() -> parent.removeView(child));
                mCounter.decrementAndGet();
            }

            @Override
            public void onAnimationRepeat(Animation animation) {

            }

            @Override
            public void onAnimationStart(Animation animation) {
                mCounter.incrementAndGet();
            }
        });
        child.startAnimation(anim);
    }

    static class FloatAnimation extends Animation {
        private final PathMeasure mPm;
        private final View        mView;
        private final float       mDistance;
        private final float       mRotation;

        public FloatAnimation(Path path, float rotation, View parent, View child) {
            mPm = new PathMeasure(path, false);
            mDistance = mPm.getLength();
            mView = child;
            mRotation = rotation;
            parent.setLayerType(View.LAYER_TYPE_SOFTWARE, null);
        }

        @Override
        protected void applyTransformation(float factor, Transformation transformation) {
            Matrix matrix = transformation.getMatrix();
            mPm.getMatrix(mDistance * factor, matrix, PathMeasure.POSITION_MATRIX_FLAG);
            mView.setRotation(mRotation * factor);
            float scale = 1F;
            if (3000.0F * factor < 200.0F) {
                scale = scale(factor, 0.0D, 0.06666667014360428D, 0.20000000298023224D, 1.100000023841858D);
            } else if (3000.0F * factor < 300.0F) {
                scale = scale(factor, 0.06666667014360428D, 0.10000000149011612D, 1.100000023841858D, 1.0D);
            }
            mView.setScaleX(scale);
            mView.setScaleY(scale);
            transformation.setAlpha(1.0F - factor);
        }
    }

    private static float scale(double a, double b, double c, double d, double e) {
        return (float) ((a - b) / (c - b) * (e - d) + d);
    }
}

