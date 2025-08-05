package com.tencent.cloud.uikit.livekit.karaoke.manager

import com.google.gson.Gson
import com.tencent.trtc.TRTCCloud

fun setAudioEffect(trtcCloud: TRTCCloud) {
    val gson = Gson()

    val jsonDsp = gson.toJson(
        mapOf(
            "api" to "setPrivateConfig",
            "params" to mapOf(
                "configs" to listOf(
                    mapOf(
                        "key" to "Liteav.Audio.common.dsp.version",
                        "value" to "2",
                        "default" to "1"
                    )
                )
            )
        )
    )
    trtcCloud.callExperimentalAPI(jsonDsp)

    val jsonHifi = gson.toJson(
        mapOf(
            "api" to "setPrivateConfig",
            "params" to mapOf(
                "configs" to listOf(
                    mapOf(
                        "key" to "Liteav.Audio.common.smart.3a.strategy.flag",
                        "value" to "16",
                        "default" to "1"
                    )
                )
            )
        )
    )
    trtcCloud.callExperimentalAPI(jsonHifi)

    val jsonAiecModel2 = gson.toJson(
        mapOf(
            "api" to "setPrivateConfig",
            "params" to mapOf(
                "configs" to listOf(
                    mapOf(
                        "key" to "Liteav.Audio.common.ai.ec.model.type",
                        "value" to "2",
                        "default" to "2"
                    )
                )
            )
        )
    )
    trtcCloud.callExperimentalAPI(jsonAiecModel2)

    val jsonEnableAiec = gson.toJson(
        mapOf(
            "api" to "setPrivateConfig",
            "params" to mapOf(
                "configs" to listOf(
                    mapOf(
                        "key" to "Liteav.Audio.common.enable.ai.ec.module",
                        "value" to "1",
                        "default" to "1"
                    )
                )
            )
        )
    )
    trtcCloud.callExperimentalAPI(jsonEnableAiec)

    val jsonEnableAiModule = gson.toJson(
        mapOf(
            "api" to "setPrivateConfig",
            "params" to mapOf(
                "configs" to listOf(
                    mapOf(
                        "key" to "Liteav.Audio.common.ai.module.enabled",
                        "value" to "1",
                        "default" to "1"
                    )
                )
            )
        )
    )
    trtcCloud.callExperimentalAPI(jsonEnableAiModule)

    val reverGson = Gson()
    val customReverbParams = mapOf(
        "enable" to true,
        "RoomSize" to 60,
        "PreDelay" to 20,
        "Reverberance" to 40,
        "Damping" to 50,
        "ToneLow" to 30,
        "ToneHigh" to 100,
        "WetGain" to -3,
        "DryGain" to 0,
        "StereoWidth" to 40,
        "WetOnly" to false
    )
    val jsonCustomReverb = reverGson.toJson(
        mapOf(
            "api" to "setCustomReverbParams",
            "params" to customReverbParams
        )
    )
    trtcCloud.callExperimentalAPI(jsonCustomReverb)

    val jsonSetChorusBgmDelay = gson.toJson(
        mapOf(
            "api" to "setChorusBgmDelay",
            "params" to mapOf(
                "delay" to 200
            )
        )
    )
    trtcCloud.callExperimentalAPI(jsonSetChorusBgmDelay)
}