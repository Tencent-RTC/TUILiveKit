package com.trtc.uikit.livekit.livestreamcore.common.utils;

import android.graphics.Color;
import android.text.TextUtils;
import android.util.Log;

public class ColorUtil {

    public static int parseHex(String hexColor) {
        if (TextUtils.isEmpty(hexColor)) {
            return 0;
        }
        String colorString = hexColor;
        if (hexColor.length() >= 8 && hexColor.substring(0, 2).equalsIgnoreCase("0x")) {
            colorString = "#" + hexColor.substring(2);
        }
        if (colorString.startsWith("#")) {
            try {
                return Color.parseColor(colorString);
            } catch (Exception e) {
                Log.i("ColorUtil", e.toString());
            }
        }
        return 0;
    }
}
