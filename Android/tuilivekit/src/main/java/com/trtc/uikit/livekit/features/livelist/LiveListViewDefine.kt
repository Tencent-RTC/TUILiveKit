package com.trtc.uikit.livekit.features.livelist

import android.view.View
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager

enum class Style {
    SINGLE_COLUMN,
    DOUBLE_COLUMN
}

data class FetchLiveListParam(
    var cursor: String
)

interface LiveListCallback {
    fun onSuccess(cursor: String, liveInfoList: List<TUILiveListManager.LiveInfo>)
    fun onError(code: Int, message: String)
}

interface LiveListViewAdapter {
    fun createLiveInfoView(liveInfo: TUILiveListManager.LiveInfo): View
    fun updateLiveInfoView(view: View, liveInfo: TUILiveListManager.LiveInfo)
}

interface LiveListDataSource {
    fun fetchLiveList(param: FetchLiveListParam, callback: LiveListCallback)
}

fun interface OnItemClickListener {
    fun onItemClick(view: View, liveInfo: TUILiveListManager.LiveInfo)
}
