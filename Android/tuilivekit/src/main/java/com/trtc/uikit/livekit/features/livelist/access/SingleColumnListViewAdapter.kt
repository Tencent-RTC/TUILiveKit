package com.trtc.uikit.livekit.features.livelist.access

import android.content.Context
import android.view.View
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.trtc.uikit.livekit.features.livelist.LiveListViewAdapter

class SingleColumnListViewAdapter(private val context: Context) : LiveListViewAdapter {

    override fun createLiveInfoView(liveInfo: TUILiveListManager.LiveInfo): View {
        val widgetView = SingleColumnWidgetView(context)
        widgetView.init(liveInfo)
        return widgetView
    }

    override fun updateLiveInfoView(view: View, liveInfo: TUILiveListManager.LiveInfo) {
        val widgetView = view as SingleColumnWidgetView
        widgetView.updateLiveInfoView(liveInfo)
    }
}
