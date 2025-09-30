package com.trtc.uikit.livekit.component.beauty

import android.content.Context
import android.os.Bundle
import android.view.View
import com.trtc.tuikit.common.ui.PopupDialog
import com.trtc.uikit.livekit.component.beauty.basicbeauty.BeautyListPanel
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyView

class BeautyPanelDialog(
    private val context: Context
) : PopupDialog(context, com.trtc.tuikit.common.R.style.TUICommonBottomDialogTheme) {

    override fun onCreate(savedInstanceState: Bundle?) {
        val beautyView: View = if (TEBeautyManager.isSupportTEBeauty()) {
            TEBeautyView(context)
        } else {
            BeautyListPanel(context)
        }
        setView(beautyView)
        super.onCreate(savedInstanceState)
    }
}
