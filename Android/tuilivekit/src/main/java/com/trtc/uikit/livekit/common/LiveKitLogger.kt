package com.trtc.uikit.livekit.common

import android.util.Log
import com.tencent.trtc.TRTCCloud
import com.trtc.tuikit.common.system.ContextProvider
import org.json.JSONException
import org.json.JSONObject

class LiveKitLogger private constructor(private val moduleName: String, private val fileName: String) {

    companion object {
        const val MODULE_LIVE_STREAM_CORE = "KitLiveStream"
        const val MODULE_SEAT_GRID_CORE = "KitVoiceRoom"
        const val MODULE_FEATURES = "KitFeatures"
        const val MODULE_COMPONENT = "KitComponent"
        const val MODULE_COMMON = "KitCommon"

        private const val API = "TuikitLog"
        private const val LOG_KEY_API = "api"
        private const val LOG_KEY_PARAMS = "params"
        private const val LOG_KEY_PARAMS_LEVEL = "level"
        private const val LOG_KEY_PARAMS_MESSAGE = "message"
        private const val LOG_KEY_PARAMS_FILE = "file"
        private const val LOG_KEY_PARAMS_MODULE = "module"
        private const val LOG_KEY_PARAMS_LINE = "line"
        private const val LOG_LEVEL_INFO = 0
        private const val LOG_LEVEL_WARNING = 1
        private const val LOG_LEVEL_ERROR = 2

        @JvmStatic
        fun getLiveStreamLogger(file: String) = LiveKitLogger(MODULE_LIVE_STREAM_CORE, file)
        @JvmStatic
        fun getVoiceRoomLogger(file: String) = LiveKitLogger(MODULE_SEAT_GRID_CORE, file)
        @JvmStatic
        fun getFeaturesLogger(file: String) = LiveKitLogger(MODULE_FEATURES, file)
        @JvmStatic
        fun getComponentLogger(file: String) = LiveKitLogger(MODULE_COMPONENT, file)
        @JvmStatic
        fun getCommonLogger(file: String) = LiveKitLogger(MODULE_COMMON, file)

        private fun log(module: String, file: String, level: Int, message: String) = try {
            JSONObject().apply {
                put(LOG_KEY_API, API)
                put(LOG_KEY_PARAMS, JSONObject().apply {
                    put(LOG_KEY_PARAMS_LEVEL, level)
                    put(LOG_KEY_PARAMS_MESSAGE, message)
                    put(LOG_KEY_PARAMS_MODULE, module)
                    put(LOG_KEY_PARAMS_FILE, file)
                    put(LOG_KEY_PARAMS_LINE, 0)
                })
            }.toString().also { json ->
                TRTCCloud.sharedInstance(ContextProvider.getApplicationContext()).callExperimentalAPI(json)
            }
        } catch (e: JSONException) {
            Log.e("Logger", e.toString())
        }
    }

    fun info(message: String) = log(LOG_LEVEL_INFO, message)
    fun warn(message: String) = log(LOG_LEVEL_WARNING, message)
    fun error(message: String) = log(LOG_LEVEL_ERROR, message)

    private fun log(level: Int, message: String) = log(moduleName, fileName, level, message)
}