package com.trtc.uikit.livekit.livestream.view.audience.playing;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ObjectAnimator;
import android.content.Context;
import android.util.AttributeSet;
import android.view.GestureDetector;
import android.view.MotionEvent;
import android.view.View;
import android.view.animation.LinearInterpolator;
import android.widget.FrameLayout;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.trtc.uikit.livekit.R;

public class AudiencePlayingRootView extends FrameLayout {

    private static final int                MOVE_ANIMATION_DURATION_MS  = 200;
    private static final LinearInterpolator MOVE_ANIMATION_INTERPOLATOR = new LinearInterpolator();

    private final GestureDetector mGestureDetector;
    private       View            mPlayingView;
    private       FrameLayout     mRecoverLayout;
    private       ImageView       mRecoverImageView;

    public AudiencePlayingRootView(@NonNull Context context) {
        this(context, null);
    }

    public AudiencePlayingRootView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        mGestureDetector = new GestureDetector(context, new GestureDetector.SimpleOnGestureListener() {
            @Override
            public boolean onFling(@Nullable MotionEvent e1, @NonNull MotionEvent e2, float velocityX, float velocityY) {
                if (velocityX > 0) {
                    // Move Right
                    startMoveXAnimation(mPlayingView.getX(), mPlayingView.getWidth());
                    return true;
                } else if (velocityX < 0) {
                    // Move Left
                    startMoveXAnimation(mPlayingView.getX(), 0);
                    return true;
                } else {
                    return super.onFling(e1, e2, velocityX, velocityY);
                }
            }

            @Override
            public boolean onScroll(@Nullable MotionEvent e1, @NonNull MotionEvent e2, float distanceX, float distanceY) {
                if (mPlayingView != null) {
                    float newX = mPlayingView.getX() - distanceX;
                    if (newX >= 0 && newX <= mPlayingView.getWidth()) {
                        mPlayingView.setX(newX);
                        return true;
                    }
                }
                return super.onScroll(e1, e2, distanceX, distanceY);
            }
        });
    }

    @Override
    protected void onFinishInflate() {
        super.onFinishInflate();
        mPlayingView = findViewById(R.id.fl_playing);
        mRecoverLayout = findViewById(R.id.fl_recover);
        mRecoverImageView = findViewById(R.id.iv_recover);

        mRecoverLayout.setVisibility(GONE);
        mRecoverImageView.setOnClickListener(v -> {
            if (!mRecoverImageView.isEnabled()) {
                return;
            }
            mRecoverImageView.setEnabled(false);
            startMoveXAnimation(mPlayingView.getX(), 0);
        });
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (mGestureDetector.onTouchEvent(event)) {
            getParent().requestDisallowInterceptTouchEvent(true);
            return true;
        }
        final int action = event.getAction();
        if (action == MotionEvent.ACTION_UP || action == MotionEvent.ACTION_CANCEL) {
            float w = mPlayingView.getWidth();
            float startX = mPlayingView.getX();
            float endX = startX > w / 2 ? w : 0;
            startMoveXAnimation(startX, endX);
        }
        return super.onTouchEvent(event);
    }

    private void startMoveXAnimation(float startX, float endX) {
        ObjectAnimator animator = ObjectAnimator.ofFloat(mPlayingView, "X", startX, endX);
        animator.setInterpolator(MOVE_ANIMATION_INTERPOLATOR);
        animator.setDuration(MOVE_ANIMATION_DURATION_MS);
        animator.addListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationCancel(Animator animation) {
                onFinished(true);
            }

            @Override
            public void onAnimationEnd(Animator animation) {
                onFinished(false);
            }

            private void onFinished(boolean cancel) {
                mRecoverImageView.setEnabled(true);
                float end = cancel ? mPlayingView.getX() : endX;
                if (end == mPlayingView.getWidth()) {
                    mRecoverLayout.setVisibility(VISIBLE);
                } else {
                    mRecoverLayout.setVisibility(GONE);
                }
            }
        });
        animator.start();
    }
}
