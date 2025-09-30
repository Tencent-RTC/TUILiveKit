package com.trtc.uikit.livekit.livestream

import android.content.Context
import android.content.Intent
import android.graphics.Rect
import android.os.Bundle
import android.view.MotionEvent
import android.view.View
import android.view.View.GONE
import android.view.View.VISIBLE
import android.view.inputmethod.InputMethodManager
import android.widget.EditText
import android.widget.ImageView
import androidx.activity.addCallback
import androidx.constraintlayout.widget.ConstraintLayout
import androidx.constraintlayout.widget.ConstraintSet
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.qcloud.tuicore.TUIConstants
import com.tencent.qcloud.tuicore.TUICore
import com.tencent.qcloud.tuicore.TUILogin
import com.tencent.qcloud.tuicore.util.SPUtils
import com.trtc.tuikit.common.FullScreenActivity
import com.trtc.tuikit.common.util.ToastUtil
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.EVENT_KEY_LIVE_KIT
import com.trtc.uikit.livekit.common.EVENT_SUB_KEY_DESTROY_LIVE_VIEW
import com.trtc.uikit.livekit.common.LiveIdentityGenerator
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.component.pictureinpicture.PictureInPictureStore
import com.trtc.uikit.livekit.features.livelist.LiveListView
import com.trtc.uikit.livekit.features.livelist.Style
import com.trtc.uikit.livekit.voiceroom.VoiceRoomKit

class VideoLiveListActivity : FullScreenActivity() {

    companion object {
        const val EVENT_ADVANCE_SETTING_EXTENSION = "AdvanceSettingExtension"
        const val EVENT_SUB_KEY_SHOW_ADVANCE_SETTING_VIEW = "showAdvanceSettingView"
        const val EVENT_SUB_KEY_HIDE_ADVANCE_SETTING_VIEW = "hideAdvanceSettingView"
        const val EVENT_SUB_KEY_REAL_NAME_VERIFY = "eventRealNameVerify"

        private val LOGGER = LiveKitLogger.getComponentLogger("VideoLiveListActivity")
    }

    private var isAdvanceSettingsViewVisible = false
    private var mStyle: Style = Style.DOUBLE_COLUMN
    private lateinit var mMainLayout: ConstraintLayout
    private lateinit var mLiveListView: LiveListView
    private lateinit var mToolbarLiveView: View
    private lateinit var mStartLiveView: View
    private lateinit var mLiveListColumnTypeView: ImageView

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.livekit_activity_video_live_list)
        window.decorView.systemUiVisibility = View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN

        mMainLayout = findViewById(R.id.main)
        mLiveListView = findViewById(R.id.live_list_view)
        mToolbarLiveView = findViewById(R.id.toolbar_live)
        mStartLiveView = findViewById(R.id.ll_start)
        mLiveListColumnTypeView = findViewById(R.id.btn_live_list_column_type)

        mLiveListColumnTypeView.setOnClickListener { changeColumnStyle() }
        initStartLiveView()
        initBackButton()
        initVideoLiveTitle()
        initLiveListView()
        onBackPressedDispatcher.addCallback(this) {
            if (PictureInPictureStore.sharedInstance().state.anchorIsPictureInPictureMode
                || PictureInPictureStore.sharedInstance().state.audienceIsPictureInPictureMode
            ) {
                val homeIntent = Intent(Intent.ACTION_MAIN).apply {
                    addCategory(Intent.CATEGORY_HOME)
                    flags = Intent.FLAG_ACTIVITY_NEW_TASK
                }
                startActivity(homeIntent)
            }
        }
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

    private fun initVideoLiveTitle() {
        findViewById<View>(R.id.tv_title).setOnLongClickListener {
            if (isAdvanceSettingsViewVisible) {
                hideAdvanceSettingView()
            } else {
                showAdvanceSettingView()
            }
            isAdvanceSettingsViewVisible = !isAdvanceSettingsViewVisible
            false
        }
    }

    private fun initLiveListView() {
        mLiveListView.init(this, mStyle)
        updateLiveStyleUI(mStyle)
        mLiveListView.setOnItemClickListener { view, liveInfo ->
            if (!view.isEnabled) return@setOnItemClickListener
            view.isEnabled = false
            view.postDelayed({ view.isEnabled = true }, 1000)
            enterRoom(liveInfo)
        }
    }

    private fun changeColumnStyle() {
        mStyle = if (mStyle == Style.DOUBLE_COLUMN) Style.SINGLE_COLUMN else Style.DOUBLE_COLUMN
        updateLiveStyleUI(mStyle)
    }

    private fun updateLiveStyleUI(style: Style) {
        val constraintSet = ConstraintSet()
        constraintSet.clone(mMainLayout)
        if (style == Style.DOUBLE_COLUMN) {
            constraintSet.connect(mLiveListView.id, ConstraintSet.TOP, mToolbarLiveView.id, ConstraintSet.BOTTOM)
            mLiveListColumnTypeView.setImageResource(R.drawable.livekit_ic_single_item_type)
            constraintSet.setVisibility(mStartLiveView.id, VISIBLE)
        } else {
            constraintSet.connect(mLiveListView.id, ConstraintSet.TOP, ConstraintSet.PARENT_ID, ConstraintSet.TOP)
            mLiveListColumnTypeView.setImageResource(R.drawable.livekit_ic_double_item_type)
            constraintSet.setVisibility(mStartLiveView.id, GONE)
        }
        constraintSet.applyTo(mMainLayout)
        mStyle = style
        mLiveListView.updateColumnStyle(style)
    }

    private fun initStartLiveView() {
        mStartLiveView.setOnClickListener {
            if (packageName == "com.tencent.trtc") {
                realNameVerifyAndStartLive()
                return@setOnClickListener
            }
            startVideoLive()
        }
    }

    private fun realNameVerifyAndStartLive() {
        if (SPUtils.getInstance("sp_verify").getBoolean("sp_verify", false)) {
            startVideoLive()
        } else {
            try {
                val map = HashMap<String, Any>()
                map[TUIConstants.Privacy.PARAM_DIALOG_CONTEXT] = this
                TUICore.notifyEvent(TUIConstants.Privacy.EVENT_ROOM_STATE_CHANGED, EVENT_SUB_KEY_REAL_NAME_VERIFY, map)
            } catch (e: Exception) {
                LOGGER.error("real name verify fail, exception: ${e.message}")
            }
        }
    }

    private fun startVideoLive() {
        if (PictureInPictureStore.sharedInstance().state.isAnchorStreaming) {
            ToastUtil.toastShortMessage(getString(R.string.common_exit_float_window_tip))
            return
        }
        TUICore.notifyEvent(EVENT_KEY_LIVE_KIT, EVENT_SUB_KEY_DESTROY_LIVE_VIEW, null)
        val roomId = LiveIdentityGenerator.generateId(TUILogin.getUserId(), LiveIdentityGenerator.RoomType.LIVE)
        VideoLiveKit.createInstance(applicationContext).startLive(roomId)
    }

    private fun initBackButton() {
        findViewById<View>(R.id.iv_back).setOnClickListener {
            hideAdvanceSettingView()
            if (PictureInPictureStore.sharedInstance().state.anchorIsPictureInPictureMode
                || PictureInPictureStore.sharedInstance().state.audienceIsPictureInPictureMode
            ) {
                val homeIntent = Intent(Intent.ACTION_MAIN).apply {
                    addCategory(Intent.CATEGORY_HOME)
                    flags = Intent.FLAG_ACTIVITY_NEW_TASK
                }
                startActivity(homeIntent)
                return@setOnClickListener
            }
            finish()
        }
    }

    private fun showAdvanceSettingView() {
        TUICore.notifyEvent(EVENT_ADVANCE_SETTING_EXTENSION, EVENT_SUB_KEY_SHOW_ADVANCE_SETTING_VIEW, null)
    }

    private fun hideAdvanceSettingView() {
        TUICore.notifyEvent(EVENT_ADVANCE_SETTING_EXTENSION, EVENT_SUB_KEY_HIDE_ADVANCE_SETTING_VIEW, null)
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