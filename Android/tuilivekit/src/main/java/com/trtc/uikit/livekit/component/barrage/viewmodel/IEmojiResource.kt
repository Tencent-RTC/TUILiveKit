package com.trtc.uikit.livekit.component.barrage.viewmodel

import android.content.Context
import android.graphics.Rect
import android.graphics.drawable.Drawable

interface IEmojiResource {
    fun getResId(key: String): Int

    fun getResIds(): List<Int>

    fun getEncodeValue(resId: Int): String

    fun getEncodePattern(): String

    fun getDrawable(context: Context, resId: Int, bounds: Rect?): Drawable
}
