package com.trtc.uikit.livekit.component.beauty.tebeauty

import android.content.Context
import android.os.Bundle
import android.os.Handler
import android.os.Looper
import android.view.View
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.qcloud.tuicore.TUICore
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback
import com.tencent.trtc.TRTCCloudDef
import com.tencent.trtc.TRTCCloudListener.TRTCVideoFrameListener
import com.trtc.tuikit.common.system.ContextProvider
import com.trtc.uikit.livekit.common.LiveKitLogger
import java.lang.ref.WeakReference

object TEBeautyManager {
    private const val TE_BEAUTY_EXTENSION = "TEBeautyExtension"
    private val LOGGER = LiveKitLogger.getComponentLogger("TEBeautyManager")

    private var listenerRef: WeakReference<OnBeautyListener>? = null
    private var lastParamList: String? = null
    private val mainHandler = Handler(Looper.getMainLooper())

    private var hasLoaded = false

    private val trtcVideoFrameListener = object : TRTCVideoFrameListener {
        override fun onGLContextCreated() {
            if (hasLoaded) {
                mainHandler.post { initBeautyKit(ContextProvider.getApplicationContext(), false) }
            }
        }

        override fun onProcessVideoFrame(
            srcFrame: TRTCCloudDef.TRTCVideoFrame,
            dstFrame: TRTCCloudDef.TRTCVideoFrame
        ): Int {
            val params = mapOf(
                "srcTextureId" to srcFrame.texture.textureId,
                "frameWidth" to srcFrame.width,
                "frameHeight" to srcFrame.height
            )
            dstFrame.texture.textureId = TUICore.callService(TE_BEAUTY_EXTENSION, "processVideoFrame", params) as Int
            return 0
        }

        override fun onGLContextDestory() {
            val lastParam = exportParam()
            destroyBeautyKit()
            mainHandler.post {
                lastParamList = lastParam
                listenerRef?.get()?.onDestroyBeautyView()
            }
        }
    }

    fun isSupportTEBeauty(): Boolean = TUICore.getService(TE_BEAUTY_EXTENSION) != null

    fun setCustomVideoProcess() {
        lastParamList = ""
        if (isSupportTEBeauty()) {
            TUIRoomEngine.sharedInstance().getTRTCCloud().setLocalVideoProcessListener(
                TRTCCloudDef.TRTC_VIDEO_PIXEL_FORMAT_Texture_2D,
                TRTCCloudDef.TRTC_VIDEO_BUFFER_TYPE_TEXTURE,
                trtcVideoFrameListener
            )
        }
    }

    fun clear() {
        LOGGER.info("clear")
        lastParamList = null
    }

    fun setListener(listener: OnBeautyListener?) {
        listenerRef = WeakReference(listener)
    }

    fun init(context: Context) {
        createBeautyKit(context)
    }

    private fun createBeautyKit(context: Context) {
        checkBeautyResource(context, object : TUIServiceCallback() {
            override fun onServiceCallback(errorCode: Int, errorMessage: String?, bundle: Bundle?) {
                if (errorCode == 0) {
                    initBeautyKit(context, true)
                } else {
                    LOGGER.error("checkBeautyResource failed: $errorMessage")
                }
            }
        })
    }

    private fun initBeautyKit(context: Context, createPanel: Boolean) {
        LOGGER.info("initBeautyKit")
        val params = mapOf(
            "context" to context,
            "lastParamList" to lastParamList
        )
        TUICore.callService(TE_BEAUTY_EXTENSION, "initBeautyKit", params, object : TUIServiceCallback() {
            override fun onServiceCallback(errorCode: Int, errorMessage: String?, bundle: Bundle?) {
                if (errorCode != 0) {
                    LOGGER.error("initBeautyKit failed: $errorMessage")
                    return
                }
                LOGGER.info("initBeautyKit success")
                if (createPanel) {
                    mainHandler.post {
                        val beautyPanel = createTEBeautyPanel(context)
                        beautyPanel?.let {
                            listenerRef?.get()?.onCreateBeautyView(it)
                        }
                    }
                }
            }
        })
    }

    fun checkBeautyResource(context: Context, callback: TUIServiceCallback? = null) {
        LOGGER.info("checkBeautyResource")
        val params = mapOf("context" to context)
        TUICore.callService(TE_BEAUTY_EXTENSION, "checkResource", params, object : TUIServiceCallback() {
            override fun onServiceCallback(errorCode: Int, errorMessage: String?, bundle: Bundle?) {
                hasLoaded = true
                callback?.onServiceCallback(errorCode, errorMessage, bundle)
            }
        })
    }

    private fun destroyBeautyKit() {
        LOGGER.info("destroyBeautyKit")
        TUICore.callService(TE_BEAUTY_EXTENSION, "destroyBeautyKit", null)
    }

    private fun exportParam(): String {
        val param = TUICore.callService(TE_BEAUTY_EXTENSION, "exportParam", null)?.toString() ?: "null"
        LOGGER.info("exportParam:$param")
        return param
    }

    private fun createTEBeautyPanel(context: Context): View? {
        val param = mapOf(
            "context" to context,
            "lastParamList" to lastParamList
        )
        return TUICore.getExtensionList("TEBeautyExtension", param)
            .firstNotNullOfOrNull { extensionInfo ->
                (extensionInfo.data["beautyPanel"] as? View)
            }
    }

    interface OnBeautyListener {
        fun onCreateBeautyView(view: View)
        fun onDestroyBeautyView()
    }
}
