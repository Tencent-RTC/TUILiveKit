package com.trtc.uikit.livekit.common.utils;

import android.text.TextUtils;

public class BackgroundImageUtils {
    public static String transferThumbUrlFromImage(String imageUrl) {
        if (TextUtils.isEmpty(imageUrl)) {
            return imageUrl;
        }

        int index = imageUrl.indexOf(".png");
        if (index == -1) {
            return imageUrl;
        }
        return imageUrl.substring(0, index) + "_thumb.png";
    }

    public static String transferImageUrlFromThumb(String thumbUrl) {
        if (TextUtils.isEmpty(thumbUrl)) {
            return thumbUrl;
        }

        int index = thumbUrl.indexOf("_thumb.png");
        if (index == -1) {
            return thumbUrl;
        }
        return thumbUrl.substring(0, index) + ".png";
    }
}
