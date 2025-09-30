package com.trtc.uikit.livekit.features.livelist.manager

import android.text.TextUtils
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager.LiveInfo
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.features.livelist.FetchLiveListParam
import com.trtc.uikit.livekit.features.livelist.LiveListCallback
import com.trtc.uikit.livekit.features.livelist.LiveListDataSource


class LiveInfoListService(private val liveListDataSource: LiveListDataSource) {

    companion object {
        private val LOGGER = LiveKitLogger.getComponentLogger("LiveInfoListService")
    }

    private var cursor: String = ""
    private var firstLiveInfo: LiveInfo? = null
    private val liveInfoList: MutableList<LiveInfo> = mutableListOf()

    fun refreshLiveList(callback: LiveListCallback?) {
        fetchLiveList(true, callback)
    }

    fun fetchLiveList(callback: LiveListCallback?) {
        fetchLiveList(false, callback)
    }

    private fun fetchLiveList(isRefresh: Boolean, callback: LiveListCallback?) {
        LOGGER.info("fetchLiveList start,isRefresh:$isRefresh")
        val param = FetchLiveListParam(cursor = if (isRefresh || cursor.isEmpty()) "" else cursor)
        cursor = param.cursor
        liveListDataSource.fetchLiveList(param, object : LiveListCallback {
            override fun onSuccess(cursor: String, liveInfoList: List<LiveInfo>) {
                LOGGER.info("fetchLiveList onSuccess. result.liveInfoList.size:${liveInfoList.size}")
                val list = ArrayList<LiveInfo>()
                for (info in liveInfoList) {
                    val first = firstLiveInfo
                    if (first != null && TextUtils.equals(info.roomId, first.roomId)) {
                        continue
                    }
                    list.add(info)
                }
                if (isRefresh) {
                    this@LiveInfoListService.liveInfoList.clear()
                }
                this@LiveInfoListService.liveInfoList.addAll(list)
                this@LiveInfoListService.cursor = cursor
                callback?.onSuccess(cursor, list)
            }

            override fun onError(code: Int, message: String) {
                LOGGER.error("fetchLiveList onError. code:$code,message:$message")
                callback?.onError(code, message)
            }
        })
    }

    fun getLiveList(): List<LiveInfo> = liveInfoList

    fun getLiveListDataCursor(): String = cursor
}
