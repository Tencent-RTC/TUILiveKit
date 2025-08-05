package com.tencent.cloud.uikit.livekit.utils

import android.graphics.Color
import android.view.View
import android.view.Window
import android.view.WindowManager
import androidx.activity.ComponentActivity

object ActivityUtils {
    fun setActivityFullScreen(activity: ComponentActivity, isLightBack: Boolean) {
        val window: Window = activity.getWindow()
        window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS)
        if (isLightBack) {
            window.getDecorView().setSystemUiVisibility(
                View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN or View.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR
            )
        } else {
            window.getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN)
        }
        window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS)
        window.setStatusBarColor(Color.TRANSPARENT)
    }
}
