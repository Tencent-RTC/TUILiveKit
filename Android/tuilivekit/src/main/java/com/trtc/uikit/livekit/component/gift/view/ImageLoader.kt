package com.trtc.uikit.livekit.component.gift.view

import android.content.Context
import android.content.res.Resources
import android.graphics.Bitmap
import android.graphics.BitmapShader
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.RectF
import android.graphics.Shader
import android.graphics.drawable.Drawable
import android.text.TextUtils
import android.widget.ImageView
import androidx.annotation.DrawableRes
import com.bumptech.glide.Glide
import com.bumptech.glide.RequestBuilder
import com.bumptech.glide.load.engine.bitmap_recycle.BitmapPool
import com.bumptech.glide.load.resource.bitmap.BitmapTransformation
import com.bumptech.glide.request.RequestOptions
import java.security.MessageDigest

object ImageLoader {
    private const val radius = 15

    fun clear(context: Context, imageView: ImageView) {
        Glide.with(context).clear(imageView)
    }

    @JvmOverloads
    fun loadImage(
        context: Context, imageView: ImageView, url: String?, @DrawableRes errorResId: Int = 0,
        radius: Int = ImageLoader.radius
    ) {
        if (TextUtils.isEmpty(url)) {
            if (errorResId != 0) {
                imageView.setImageResource(errorResId)
            }
            return
        }
        Glide.with(context).load(url).error(loadTransform(context, errorResId, radius)).into(imageView)
    }

    private fun loadTransform(
        context: Context,
        @DrawableRes placeholderId: Int,
        radius: Int
    ): RequestBuilder<Drawable?> {
        return Glide.with(context).load(placeholderId)
            .apply(RequestOptions().centerCrop().transform(GlideRoundTransform(radius)))
    }

    class GlideRoundTransform(dp: Int) : BitmapTransformation() {
        init {
            radius = Resources.getSystem().displayMetrics.density * dp
        }

        override fun transform(pool: BitmapPool, toTransform: Bitmap, outWidth: Int, outHeight: Int): Bitmap? {
            return roundCrop(pool, toTransform)
        }

        override fun updateDiskCacheKey(messageDigest: MessageDigest) {
        }

        companion object {
            private var radius = 0f

            private fun roundCrop(pool: BitmapPool, source: Bitmap?): Bitmap? {
                if (source == null) {
                    return null
                }
                val result = pool.get(source.getWidth(), source.getHeight(), Bitmap.Config.ARGB_8888)
                val canvas = Canvas(result)
                val paint = Paint()
                paint.setShader(BitmapShader(source, Shader.TileMode.CLAMP, Shader.TileMode.CLAMP))
                paint.isAntiAlias = true
                val rectF = RectF(0f, 0f, source.getWidth().toFloat(), source.getHeight().toFloat())
                canvas.drawRoundRect(rectF, radius, radius, paint)
                return result
            }
        }
    }
}