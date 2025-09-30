package com.trtc.uikit.livekit.common

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import io.trtc.tuikit.atomicxcore.api.CompletionHandler

class TUIActionCallback(
    private val success: () -> Unit = {},
    private val error: (TUICommonDefine.Error, String) -> Unit = { _, _ -> }
) : TUIRoomDefine.ActionCallback {

    override fun onSuccess() {
        success()
    }

    override fun onError(errorCode: TUICommonDefine.Error, message: String) {
        error(errorCode, message)
    }
}

class CompletionHandlerBuilder {
    private var _onSuccess: () -> Unit = {}
    private var _onError: (Int, String) -> Unit = { _, _ -> }

    fun onSuccess(block: () -> Unit) {
        _onSuccess = block
    }

    fun onError(block: (Int, String) -> Unit) {
        _onError = block
    }

    fun build(): CompletionHandler {
        return object : CompletionHandler {
            override fun onSuccess() = _onSuccess()
            override fun onFailure(code: Int, desc: String) = _onError(code, desc)
        }
    }
}

fun completionHandler(block: CompletionHandlerBuilder.() -> Unit): CompletionHandler {
    return CompletionHandlerBuilder().apply(block).build()
}