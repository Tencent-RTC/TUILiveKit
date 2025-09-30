package com.trtc.uikit.livekit.features.livelist.access

import android.text.TextUtils
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine.Error.SDK_NOT_INITIALIZED
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine.ExtensionType.LIVE_LIST_MANAGER
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.trtc.uikit.livekit.common.ErrorLocalized
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.features.livelist.FetchLiveListParam
import com.trtc.uikit.livekit.features.livelist.LiveListCallback
import com.trtc.uikit.livekit.features.livelist.LiveListDataSource

class TUILiveListDataSource : LiveListDataSource {

    companion object {
        private val LOGGER = LiveKitLogger.getComponentLogger("TUILiveListDataSource")
        private const val FETCH_LIST_COUNT = 20
    }

    override fun fetchLiveList(param: FetchLiveListParam, callback: LiveListCallback) {
        val userInfo = TUIRoomEngine.getSelfInfo()
        if (userInfo == null || TextUtils.isEmpty(userInfo.userId)) {
            LOGGER.warn("TUIRoomEngine login first")
            callback.onError(SDK_NOT_INITIALIZED.value, "message")
            return
        }
        val engine = TUIRoomEngine.sharedInstance()
        val manager = engine.getExtension(LIVE_LIST_MANAGER) as TUILiveListManager
        manager.fetchLiveList(param.cursor, FETCH_LIST_COUNT, object : TUILiveListManager.LiveInfoListCallback {
            override fun onSuccess(result: TUILiveListManager.LiveInfoListResult) {
                LOGGER.info("fetchLiveList onSuccess. result.liveInfoList.size:${result.liveInfoList.size}")
                callback.onSuccess(result.cursor, result.liveInfoList)
            }

            override fun onError(error: TUICommonDefine.Error, message: String) {
                LOGGER.error("fetchLiveList failed:error:$error,errorCode:${error.value},message:$message")
                ErrorLocalized.onError(error)
                callback.onError(error.value, message)
            }
        })
    }
}
