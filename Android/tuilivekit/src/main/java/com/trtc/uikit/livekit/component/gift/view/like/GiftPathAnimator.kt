package com.trtc.uikit.livekit.component.gift.view.like

import android.graphics.Path
import android.graphics.PathMeasure
import android.view.View
import android.view.ViewGroup
import android.view.animation.Animation
import android.view.animation.LinearInterpolator
import android.view.animation.Transformation
import java.security.SecureRandom
import java.util.concurrent.atomic.AtomicInteger
import kotlin.math.abs

class GiftPathAnimator(config: Config) : GiftAbstractPathAnimator(config) {
    private var mCurrentPathCounts = 0
    private val mPathVec: HashMap<Int?, Path?> = HashMap<Int?, Path?>()
    private val mRandom: SecureRandom = SecureRandom()
    private val mCounter = AtomicInteger(0)

    override fun start(child: View, parent: ViewGroup) {
        parent.addView(child, ViewGroup.LayoutParams(config.heartWidth, config.heartHeight))

        val path: Path?
        ++mCurrentPathCounts

        if (mCurrentPathCounts > MAX_PATH_COUNTS) {
            path = mPathVec.get(abs(mRandom.nextInt() % MAX_PATH_COUNTS) + 1)
        } else {
            path = createPath(mCounter, parent, 2)
            mPathVec.put(mCurrentPathCounts, path)
        }

        val anim = FloatAnimation(path, randomRotation(), parent, child)
        anim.setDuration(config.animDuration.toLong())
        anim.setInterpolator(LinearInterpolator())
        anim.setAnimationListener(object : Animation.AnimationListener {
            override fun onAnimationEnd(animation: Animation?) {
                child.setAlpha(0f)
                parent.removeView(child)
                mCounter.decrementAndGet()
            }

            override fun onAnimationRepeat(animation: Animation?) {
            }

            override fun onAnimationStart(animation: Animation?) {
                child.setAlpha(1f)
                mCounter.incrementAndGet()
            }
        })
        child.startAnimation(anim)
    }

    internal class FloatAnimation(path: Path?, private val mRotation: Float, parent: View, private val mView: View) :
        Animation() {
        private val mPm: PathMeasure
        private val mDistance: Float

        init {
            mPm = PathMeasure(path, false)
            mDistance = mPm.length
            parent.setLayerType(View.LAYER_TYPE_SOFTWARE, null)
        }

        override fun applyTransformation(factor: Float, transformation: Transformation) {
            val matrix = transformation.matrix
            mPm.getMatrix(mDistance * factor, matrix, PathMeasure.POSITION_MATRIX_FLAG)
            mView.rotation = mRotation * factor
            var scale = 1f
            if (3000.0f * factor < 200.0f) {
                scale = scale(factor.toDouble(), 0.0, 0.06666667014360428, 0.20000000298023224, 1.100000023841858)
            } else if (3000.0f * factor < 300.0f) {
                scale = scale(factor.toDouble(), 0.06666667014360428, 0.10000000149011612, 1.100000023841858, 1.0)
            }
            mView.scaleX = scale
            mView.scaleY = scale
            transformation.alpha = (1.0f - factor)
        }
    }

    companion object {
        private const val MAX_PATH_COUNTS = 10

        private fun scale(a: Double, b: Double, c: Double, d: Double, e: Double): Float {
            return ((a - b) / (c - b) * (e - d) + d).toFloat()
        }
    }
}