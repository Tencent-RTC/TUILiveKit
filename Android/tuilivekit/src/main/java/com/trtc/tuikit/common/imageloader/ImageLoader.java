package com.trtc.tuikit.common.imageloader;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.widget.ImageView;

import com.bumptech.glide.Glide;
import com.bumptech.glide.RequestBuilder;
import com.bumptech.glide.load.MultiTransformation;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.load.resource.bitmap.CenterCrop;
import com.bumptech.glide.load.resource.bitmap.RoundedCorners;
import com.bumptech.glide.request.RequestOptions;

import java.io.File;
import java.lang.ref.WeakReference;

public class ImageLoader {

    public static void load(Context context, ImageView target, Object source, int placeImage) {
        ImageOptions imageOptions = new ImageOptions.Builder().setPlaceImage(placeImage).build();
        load(context, target, source, imageOptions);
    }

    public static void load(Context context, ImageView target, Object source, ImageOptions config) {
        if (source instanceof String) {
            loadUrlIntoImageView(context, (String) source, target, config);
        } else if (source instanceof File) {
            loadFileIntoImageView(context, (File) source, target, config);
        } else if (source instanceof Uri) {
            loadUriIntoImageView(context, (Uri) source, target, config);
        } else if (source instanceof Integer) {
            loadResourceIntoImageView(context, (Integer) source, target, config);
        } else if (source instanceof byte[]) {
            loadByteArrayIntoImageView(context, (byte[]) source, target, config);
        }
    }

    private static void loadFileIntoImageView(Context context, File file, ImageView target, ImageOptions config) {
        loadImageView(context, file, target, config);
    }

    private static void loadUrlIntoImageView(Context context, String url, ImageView target, ImageOptions config) {
        loadImageView(context, url, target, config);
    }

    private static void loadResourceIntoImageView(Context context, int source, ImageView target, ImageOptions config) {
        loadImageView(context, source, target, config);
    }

    private static void loadUriIntoImageView(Context context, Uri uri, ImageView target, ImageOptions config) {
        loadImageView(context, uri, target, config);
    }

    private static void loadByteArrayIntoImageView(Context context, byte[] bytes, ImageView target, ImageOptions config) {
        loadImageView(context, bytes, target, config);
    }

    private static void loadImageView(Context context, Object source, ImageView target, ImageOptions config) {
        WeakReference<Context> weakReference = new WeakReference<>(context);
        if (weakReference.get() == null) {
            return;
        }
        Context weakContext = weakReference.get();
        RequestBuilder<Drawable> builder = Glide.with(weakContext).load(source);
        setBuilderOptions(context, builder, config);
        builder.into(target);
    }

    private static int dip2px(Context context, int dpValue) {
        float scale = context.getResources().getDisplayMetrics().density;
        return (int) (dpValue * scale + 0.5f);
    }

    private static void setBuilderOptions(Context context, RequestBuilder<Drawable> builder, ImageOptions config) {
        RequestOptions options = new RequestOptions();
        if (config.roundRadius > 0) {
            options.transform(new MultiTransformation<>(new CenterCrop(), new RoundedCorners(dip2px(context, config.roundRadius))));
        } else {
            options.transform(new MultiTransformation<>(new CenterCrop()));
        }
        if (config.placeImage != 0) {
            options.placeholder(config.placeImage);
        }
        if (config.errorImage != 0) {
            options.error(config.errorImage);
        }
        if (config.skipDiskCache) {
            options.diskCacheStrategy(DiskCacheStrategy.NONE);
        } else {
            options.diskCacheStrategy(DiskCacheStrategy.ALL);
        }
        if (config.skipMemoryCache) {
            options.skipMemoryCache(true);
        } else {
            options.skipMemoryCache(false);
        }
        builder.apply(options);
    }
}