package com.trtc.uikit.livekit.component.gift.view.like

import android.content.res.TypedArray
import android.graphics.Path
import android.view.View
import android.view.ViewGroup
import com.trtc.uikit.livekit.R
import java.security.SecureRandom
import java.util.Random
import java.util.concurrent.atomic.AtomicInteger

abstract class GiftAbstractPathAnimator(protected val config: Config) {
    private val random: SecureRandom = SecureRandom()

    fun randomRotation(): Float {
        return random.nextFloat() * 28.6f - 14.3f
    }

    fun createPath(counter: AtomicInteger, view: View, factor: Int): Path {
        var factor = factor
        val r: Random = random
        var x = r.nextInt(config.xRand)
        var x2 = r.nextInt(config.xRand)
        val y = view.getHeight() - config.initY
        var y2 = counter.toInt() * 15 + config.animLength * factor + r.nextInt(config.animLengthRand)
        factor = y2 / config.bezierFactor
        x = config.xPointFactor + x
        x2 = config.xPointFactor + x2
        val y3 = y - y2
        y2 = y - y2 / 2
        val p = Path()
        p.moveTo(config.initX.toFloat(), y.toFloat())
        p.cubicTo(
            config.initX.toFloat(),
            (y - factor).toFloat(),
            x.toFloat(),
            (y2 + factor).toFloat(),
            x.toFloat(),
            y2.toFloat()
        )
        p.moveTo(x.toFloat(), y2.toFloat())
        p.cubicTo(
            x.toFloat(),
            (y2 - factor).toFloat(),
            x2.toFloat(),
            (y3 + factor).toFloat(),
            x2.toFloat(),
            y3.toFloat()
        )
        return p
    }

    abstract fun start(child: View, parent: ViewGroup)

    class Config {
        var initX: Int = 0
        var initY: Int = 0
        var xRand: Int = 0
        var animLengthRand: Int = 0
        var bezierFactor: Int = 0
        var xPointFactor: Int = 0
        var animLength: Int = 0
        var heartWidth: Int = 0
        var heartHeight: Int = 0
        var animDuration: Int = 0

        companion object {
            fun fromTypeArray(
                typedArray: TypedArray, x: Float, y: Float, pointX: Int,
                heartWidth: Int, heartHeight: Int
            ): Config {
                val config = Config()
                val res = typedArray.getResources()
                config.initX = typedArray.getDimension(
                    R.styleable.LiveKitGiftHeartLayout_initX,
                    x
                ).toInt()
                config.initY = typedArray.getDimension(
                    R.styleable.LiveKitGiftHeartLayout_initY,
                    y
                ).toInt()
                config.xRand = typedArray.getDimension(
                    R.styleable.LiveKitGiftHeartLayout_xRand,
                    res.getDimensionPixelOffset(R.dimen.gift_heart_anim_bezier_x_rand).toFloat()
                ).toInt()
                config.animLength = typedArray.getDimension(
                    R.styleable.LiveKitGiftHeartLayout_animLength,
                    res.getDimensionPixelOffset(R.dimen.gift_heart_anim_length).toFloat()
                ).toInt()
                config.animLengthRand = typedArray.getDimension(
                    R.styleable.LiveKitGiftHeartLayout_animLengthRand,
                    res.getDimensionPixelOffset(R.dimen.gift_heart_anim_length_rand).toFloat()
                ).toInt()
                config.bezierFactor = typedArray.getInteger(
                    R.styleable.LiveKitGiftHeartLayout_bezierFactor,
                    res.getInteger(R.integer.gift_heart_anim_bezier_factor)
                )
                config.xPointFactor = pointX
                config.heartWidth = heartWidth
                config.heartHeight = heartHeight
                config.animDuration = typedArray.getInteger(
                    R.styleable.LiveKitGiftHeartLayout_anim_duration,
                    res.getInteger(R.integer.gift_heart_anim_duration)
                )
                return config
            }
        }
    }
}