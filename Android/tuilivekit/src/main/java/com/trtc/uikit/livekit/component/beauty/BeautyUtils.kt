package com.trtc.uikit.livekit.component.beauty

import android.content.Context
import android.os.Bundle
import com.tencent.qcloud.tuicore.interfaces.TUIServiceCallback
import com.tencent.qcloud.tuicore.util.ToastUtil
import com.trtc.uikit.livekit.component.beauty.tebeauty.TEBeautyManager
import io.trtc.tuikit.atomicxcore.api.BaseBeautyStore
import java.lang.ref.WeakReference

object BeautyUtils {
    private var dialogWeakRef = WeakReference<BeautyPanelDialog?>(null)

    @JvmStatic
    fun showBeautyDialog(context: Context) {
        if (TEBeautyManager.isSupportTEBeauty()) {
            TEBeautyManager.checkBeautyResource(context, object : TUIServiceCallback() {
                override fun onServiceCallback(code: Int, message: String?, bundle: Bundle?) {
                    if (code == 0) {
                        val dialog = BeautyPanelDialog(context)
                        dialogWeakRef = WeakReference(dialog)
                        dialog.show()
                    } else {
                        ToastUtil.toastShortMessage("check beauty resource failed:$code,message:$message")
                    }
                }
            })
        } else {
            val dialog = BeautyPanelDialog(context)
            dialogWeakRef = WeakReference(dialog)
            dialog.show()
        }
    }

    @JvmStatic
    fun resetBeauty() {
        BaseBeautyStore.shared().reset()
    }

    @JvmStatic
    fun dismissBeautyDialog() {
        dialogWeakRef.get()?.dismiss()
    }
}
