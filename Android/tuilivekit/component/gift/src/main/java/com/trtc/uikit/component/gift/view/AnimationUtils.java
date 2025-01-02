package com.trtc.uikit.component.gift.view;

import android.animation.AnimatorSet;
import android.animation.ObjectAnimator;
import android.animation.PropertyValuesHolder;
import android.animation.TimeInterpolator;
import android.view.View;

/**
 * Gift play animation
 */
public class AnimationUtils {
    /**
     * @param target   Play view
     * @param star     Animation start coordinates
     * @param end      Animation end coordinates
     * @param duration Animation duration
     * @return Create a left-to-right fly-in animation
     */
    public static ObjectAnimator createFadesInFromLtoR(final View target, float star, float end,
                                                       int duration, TimeInterpolator interpolator) {
        ObjectAnimator animator = ObjectAnimator.ofFloat(target, "translationX", star, end);
        animator.setInterpolator(interpolator);
        animator.setDuration(duration);
        return animator;
    }

    /**
     * @param target     Play view
     * @param star       Animation start coordinates
     * @param end        Animation end coordinates
     * @param duration   Animation duration
     * @param startDelay Animation Delay time
     * @return Fly up and fade out
     */
    public static ObjectAnimator createFadesOutAnimator(final View target, float star, float end,
                                                        int duration, int startDelay) {
        PropertyValuesHolder translationY = PropertyValuesHolder.ofFloat("translationY", star, end);
        PropertyValuesHolder alpha = PropertyValuesHolder.ofFloat("alpha", 1.0f, 0f);
        ObjectAnimator animator = ObjectAnimator.ofPropertyValuesHolder(target, translationY, alpha);
        animator.setStartDelay(startDelay);
        animator.setDuration(duration);
        return animator;
    }

    public static AnimatorSet startAnimation(ObjectAnimator animatorFirst, ObjectAnimator animatorSecond) {
        AnimatorSet animSet = new AnimatorSet();
        animSet.play(animatorFirst).before(animatorSecond);
        animSet.start();
        return animSet;
    }
}
