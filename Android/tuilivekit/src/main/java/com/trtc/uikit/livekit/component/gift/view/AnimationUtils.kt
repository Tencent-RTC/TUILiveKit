package com.trtc.uikit.livekit.component.gift.view

import android.animation.AnimatorSet
import android.animation.ObjectAnimator
import android.animation.PropertyValuesHolder
import android.animation.TimeInterpolator
import android.view.View

object AnimationUtils {
    fun createFadesInFromLtoR(
        target: View?, star: Float, end: Float,
        duration: Int, interpolator: TimeInterpolator?
    ): ObjectAnimator {
        val animator = ObjectAnimator.ofFloat(target, "translationX", star, end)
        animator.interpolator = interpolator
        animator.setDuration(duration.toLong())
        return animator
    }

    fun createFadesOutAnimator(
        target: View?, star: Float, end: Float,
        duration: Int, startDelay: Int
    ): ObjectAnimator {
        val translationY = PropertyValuesHolder.ofFloat("translationY", star, end)
        val alpha = PropertyValuesHolder.ofFloat("alpha", 1.0f, 0f)
        val animator = ObjectAnimator.ofPropertyValuesHolder(target, translationY, alpha)
        animator.setStartDelay(startDelay.toLong())
        animator.setDuration(duration.toLong())
        return animator
    }

    fun startAnimation(animatorFirst: ObjectAnimator?, animatorSecond: ObjectAnimator?): AnimatorSet {
        val animSet = AnimatorSet()
        animSet.play(animatorFirst).before(animatorSecond)
        animSet.start()
        return animSet
    }
}