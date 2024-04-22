package com.trtc.tuikit.common.imageloader

import android.content.Context
import android.net.Uri
import android.widget.ImageView
import com.bumptech.glide.Glide
import com.bumptech.glide.RequestBuilder
import com.bumptech.glide.load.MultiTransformation
import com.bumptech.glide.load.engine.DiskCacheStrategy
import com.bumptech.glide.load.resource.bitmap.CenterCrop
import com.bumptech.glide.load.resource.bitmap.RoundedCorners
import com.bumptech.glide.request.RequestOptions
import com.tencent.qcloud.tuicore.TUIConfig
import java.io.File
import java.lang.ref.WeakReference


object ImageLoader {

    @JvmStatic
    fun load(context: Context, target: ImageView, source: Any, placeImage: Int) {
        val imageOptions = ImageOptions(ImageOptions.Builder().setPlaceImage(placeImage))
        load(context, target, source, imageOptions)
    }

    @JvmStatic
    fun load(context: Context, target: ImageView, source: Any, config: ImageOptions) {
        when (source) {
            is String -> loadUrlIntoImageView(context, source, target, config)
            is File -> loadFileIntoImageView(context, source, target, config)
            is Uri -> loadUriIntoImageView(context, source, target, config)
            is Int -> loadResourceIntoImageView(context, source, target, config)
            is Array<*> -> loadByteArrayIntoImageView(
                context,
                source as Array<Byte>, target, config
            )
        }
    }

    private fun loadFileIntoImageView(
        context: Context,
        file: File,
        target: ImageView,
        config: ImageOptions,
    ) {
        loadImageView(context, file, target, config)
    }

    private fun loadUrlIntoImageView(
        context: Context,
        url: String,
        target: ImageView,
        config: ImageOptions,
    ) {
        loadImageView(context, url, target, config)
    }

    private fun loadResourceIntoImageView(
        context: Context,
        source: Int,
        target: ImageView,
        config: ImageOptions,
    ) {
        loadImageView(context, source, target, config)
    }

    private fun loadUriIntoImageView(
        context: Context,
        uri: Uri,
        target: ImageView,
        config: ImageOptions,
    ) {
        loadImageView(context, uri, target, config)
    }

    private fun loadByteArrayIntoImageView(
        context: Context,
        bytes: Array<Byte>,
        target: ImageView,
        config: ImageOptions,
    ) {
        loadImageView(context, bytes, target, config)
    }

    private fun loadImageView(
        context: Context,
        source: Any,
        target: ImageView,
        config: ImageOptions,
    ) {
        var weakReference = WeakReference(context)
        if (weakReference.get() == null) {
            return
        }
        var weakContext = weakReference.get();
        var builder = if (config.isGif) {
            Glide.with(weakContext as Context).asGif().load(source)
        } else {
            Glide.with(weakContext as Context).load(source)
        }
        setBuilderOptions(builder, config)
        builder.into(target)
    }

    private fun dip2px(dpValue: Int): Int {
        val scale = TUIConfig.getAppContext().resources.displayMetrics.density
        return (dpValue * scale + 0.5f).toInt()
    }

    private fun setBuilderOptions(builder: RequestBuilder<out Any>, config: ImageOptions) {
        var options = RequestOptions()
        if (config.roundRadius > 0) {
            options.transform(MultiTransformation(CenterCrop(), RoundedCorners(dip2px(config.roundRadius))))
        } else {
            options.transform(MultiTransformation(CenterCrop()))
        }
        if (config.placeImage != 0) {
            options.placeholder(config.placeImage)
        }
        if (config.errorImage != 0) {
            options.error(config.errorImage)
        }
        if (config.skipDiskCache) {
            options.diskCacheStrategy(DiskCacheStrategy.NONE)
        } else {
            options.diskCacheStrategy(DiskCacheStrategy.ALL)
        }
        if (config.skipMemoryCache) {
            options.skipMemoryCache(true)
        } else {
            options.skipMemoryCache(false)
        }
        builder.apply(options)
    }
}