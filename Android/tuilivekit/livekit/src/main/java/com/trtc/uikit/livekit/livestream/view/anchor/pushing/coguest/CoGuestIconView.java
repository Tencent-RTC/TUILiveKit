package com.trtc.uikit.livekit.livestream.view.anchor.pushing.coguest;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;

import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.R;

public class CoGuestIconView extends View {

    private static final int   ANIMATION_INTERVAL_MS       = 500;
    private static final int[] ANIMATION_ICON_RES_ID_ARRAY = {
            R.drawable.livekit_function_link_1,
            R.drawable.livekit_function_link_2,
            R.drawable.livekit_function_link_3,
    };
    private static final int   DEFAULT_ICON_RES_ID         = R.drawable.livekit_function_link_default;

    private int mCurrentAnimationResIndex = -1;

    private final Runnable startAnimationTask = new Runnable() {
        @Override
        public void run() {
            int index = mCurrentAnimationResIndex;
            int count = ANIMATION_ICON_RES_ID_ARRAY.length;
            if (index >= 0 && index < count) {
                setBackgroundResource(ANIMATION_ICON_RES_ID_ARRAY[index]);
                postDelayed(this, ANIMATION_INTERVAL_MS);
                mCurrentAnimationResIndex = (index + 1) % count;
            }
        }
    };

    public CoGuestIconView(Context context) {
        this(context, null);
    }

    public CoGuestIconView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        setBackgroundResource(DEFAULT_ICON_RES_ID);
    }

    public void startAnimation() {
        if (mCurrentAnimationResIndex == -1) {
            mCurrentAnimationResIndex = 0;
            post(startAnimationTask);
        }
    }

    public void stopAnimation() {
        mCurrentAnimationResIndex = -1;
        setBackgroundResource(DEFAULT_ICON_RES_ID);
        removeCallbacks(startAnimationTask);
    }
}
