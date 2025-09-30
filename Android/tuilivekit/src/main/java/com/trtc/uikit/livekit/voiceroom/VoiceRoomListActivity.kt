package com.trtc.uikit.livekit.voiceroom

import android.content.Context
import android.content.Intent
import android.graphics.Rect
import android.os.Bundle
import android.view.MotionEvent
import android.view.View
import android.view.inputmethod.InputMethodManager
import android.widget.EditText
import androidx.core.net.toUri
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.qcloud.tuicore.TUIConstants
import com.tencent.qcloud.tuicore.TUICore
import com.tencent.qcloud.tuicore.TUILogin
import com.tencent.qcloud.tuicore.util.SPUtils
import com.trtc.tuikit.common.FullScreenActivity
import com.trtc.tuikit.common.util.ActivityLauncher
import com.trtc.tuikit.common.util.ToastUtil
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.EVENT_KEY_LIVE_KIT
import com.trtc.uikit.livekit.common.EVENT_SUB_KEY_DESTROY_LIVE_VIEW
import com.trtc.uikit.livekit.common.LiveIdentityGenerator
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore
import com.trtc.uikit.livekit.features.livelist.LiveListView
import com.trtc.uikit.livekit.features.livelist.Style
import com.trtc.uikit.livekit.livestream.VideoLiveKit

class VoiceRoomListActivity : FullScreenActivity() {

    companion object {
        private const val TRTC_VOICE_ROOM_DOCUMENT_URL = "https://cloud.tencent.com/document/product/647/107969"
        private const val EVENT_SUB_KEY_REAL_NAME_VERIFY = "eventRealNameVerify"
        private val LOGGER = LiveKitLogger.getComponentLogger("VoiceRoomListActivity")
    }

    private lateinit var mLiveListView: LiveListView
    private var mIsInit = false

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.livekit_activity_voice_room_list)
        initWebsiteLinkView()
        initLiveListView()
        initCreateRoomView()
        initBackButton()
    }

    override fun onResume() {
        super.onResume()
        if (!mIsInit) {
            mIsInit = true
            return
        }
        mLiveListView.refreshData()
    }

    override fun onTouchEvent(event: MotionEvent): Boolean {
        if (event.action == MotionEvent.ACTION_DOWN) {
            val v = currentFocus
            if (v is EditText) {
                val outRect = Rect()
                v.getGlobalVisibleRect(outRect)
                if (!outRect.contains(event.rawX.toInt(), event.rawY.toInt())) {
                    v.clearFocus()
                    val imm = getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager
                    imm.hideSoftInputFromWindow(v.windowToken, 0)
                }
            }
        }
        return super.onTouchEvent(event)
    }

    private fun initWebsiteLinkView() {
        findViewById<View>(R.id.btn_multi_function).setOnClickListener {
            val intent = Intent(Intent.ACTION_VIEW).apply {
                data = TRTC_VOICE_ROOM_DOCUMENT_URL.toUri()
            }
            ActivityLauncher.startActivity(this, intent)
        }
    }

    private fun initCreateRoomView() {
        findViewById<View>(R.id.ll_start).setOnClickListener {
            if (packageName == "com.tencent.trtc") {
                realNameVerifyAndStartLive()
                return@setOnClickListener
            }
            startVoiceLive()
        }
    }

    private fun realNameVerifyAndStartLive() {
        if (SPUtils.getInstance("sp_verify").getBoolean("sp_verify", false)) {
            startVoiceLive()
        } else {
            try {
                val map = HashMap<String, Any>().apply {
                    put(TUIConstants.Privacy.PARAM_DIALOG_CONTEXT, this@VoiceRoomListActivity)
                }
                TUICore.notifyEvent(TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED, EVENT_SUB_KEY_REAL_NAME_VERIFY, map)
            } catch (e: Exception) {
                LOGGER.error("real name verify fail, exception: ${e.message}")
            }
        }
    }

    private fun startVoiceLive() {
        if (PictureInPictureStore.sharedInstance().state.isAnchorStreaming) {
            ToastUtil.toastShortMessage(getString(R.string.common_exit_float_window_tip))
            return
        }
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_LIVE_VIEW, null)
        val roomId = LiveIdentityGenerator.generateId(TUILogin.getUserId(), LiveIdentityGenerator.RoomType.VOICE)
        val voiceRoomInfo = VoiceRoomDefine.CreateRoomParams().apply {
            maxAnchorCount = VoiceRoomDefine.MAX_CONNECTED_VIEWERS_COUNT
        }
        VoiceRoomKit.createInstance(applicationContext).createRoom(roomId, voiceRoomInfo)
    }

    private fun initBackButton() {
        findViewById<View>(R.id.iv_back).setOnClickListener { finish() }
    }

    private fun initLiveListView() {
        mLiveListView = findViewById(R.id.live_list_view)
        mLiveListView.init(this, Style.DOUBLE_COLUMN)
        mLiveListView.setOnItemClickListener { view, liveInfo ->
            if (!view.isEnabled) return@setOnItemClickListener
            view.isEnabled = false
            view.postDelayed({ view.isEnabled = true }, 1000)
            enterRoom(liveInfo)
        }
    }

    private fun enterRoom(info: TUILiveListManager.LiveInfo) {
        if (PictureInPictureStore.sharedInstance().state.isAnchorStreaming) {
            ToastUtil.toastShortMessage(getString(R.string.common_exit_float_window_tip))
            return
        }
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_LIVE_VIEW, null)
        if (info.roomInfo.roomId.startsWith("voice_")) {
            VoiceRoomKit.createInstance(this).enterRoom(info)
        } else {
            VideoLiveKit.createInstance(this).joinLive(info)
        }
    }
}