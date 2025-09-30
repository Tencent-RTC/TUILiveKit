package com.trtc.uikit.livekit.component.gift.view.animation

import android.content.Context
import android.os.Bundle
import android.text.TextUtils
import android.util.AttributeSet
import android.util.Log
import android.view.View
import com.tencent.qcloud.tuicore.TUICore
import com.tencent.qcloud.tuicore.interfaces.ITUIService
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback
import com.trtc.uikit.livekit.common.reportEventData
import com.trtc.uikit.livekit.component.gift.service.GiftConstants

class TCEffectAnimationView @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null) :
    AnimationView(context, attrs) {
    private val mEffectAnimView: View?
    private val mEffectPlayerService: ITUIService?
    private val mServiceCallback: TUIServiceCallback = object : TUIServiceCallback() {
        override fun onServiceCallback(errorCode: Int, errorMessage: String?, bundle: Bundle?) {
            callback?.onFinished(errorCode)
        }
    }

    init {
        mEffectAnimView = createAnimationView()
        if (mEffectAnimView != null) {
            mEffectPlayerService = TUICore.getService(KEY_SERVICE_NAME)
            val params = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT)
            addView(mEffectAnimView, params)
        } else {
            mEffectPlayerService = null
        }
    }

    private fun createAnimationView(): View? {
        val map: MutableMap<String?, Any?> = HashMap<String?, Any?>()
        map.put(KEY_PARAM_CONTEXT, context)
        map.put(KEY_GET_VIEW, "")
        val list = TUICore.getExtensionList(KEY_EXTENSION_NAME, map)
        if (!list.isEmpty()) {
            val info = list[0]
            val data = info.data
            if (data != null && data.containsKey(KEY_GET_VIEW)) {
                val view = data.get(KEY_GET_VIEW)
                return view as View?
            }
        }
        Log.w(TAG, "TUIEffectPlayerExtension create view failed!")
        return null
    }

    override fun playAnimation(playUrl: String) {
        mEffectAnimView?.visibility = VISIBLE
        val map: MutableMap<String?, Any?> = HashMap<String?, Any?>()
        map.put(KEY_PARAM_PLAY_URL, playUrl)
        map.put(KEY_PARAM_VIEW, mEffectAnimView)
        mEffectPlayerService?.onCall(KEY_METHOD_START_PLAY, map, mServiceCallback)
        reportData()
    }

    override fun stopPlay() {
        val map: MutableMap<String?, Any?> = HashMap<String?, Any?>()
        map.put(KEY_PARAM_CLEAR_LAST_FRAME, true)
        map.put(KEY_PARAM_VIEW, mEffectAnimView)
        mEffectPlayerService?.onCall(KEY_METHOD_STOP_PLAY, map, mServiceCallback)
    }

    private fun reportData() {
        val isVoiceRoom = !TextUtils.isEmpty(roomId) && roomId.startsWith("voice_")
        var key = GiftConstants.DATA_REPORT_LIVE_GIFT_EFFECT_PLAY_COUNT
        if (isVoiceRoom) {
            key = GiftConstants.DATA_REPORT_VOICE_GIFT_EFFECT_PLAY_COUNT
        }
        reportEventData(key)
    }

    companion object {
        private const val TAG = "EffectAnimationView"

        private const val KEY_EXTENSION_NAME = "TUIEffectPlayerExtension"
        private const val KEY_SERVICE_NAME = "TUIEffectPlayerService"
        private const val KEY_GET_VIEW = "TCEffectAnimView"
        private const val KEY_METHOD_START_PLAY = "startPlay"
        private const val KEY_METHOD_STOP_PLAY = "stopPlay"
        private const val KEY_PARAM_PLAY_URL = "playUrl"
        private const val KEY_PARAM_CONTEXT = "context"
        private const val KEY_PARAM_VIEW = "view"
        private const val KEY_PARAM_CLEAR_LAST_FRAME = "clearLastFrame"
    }
}