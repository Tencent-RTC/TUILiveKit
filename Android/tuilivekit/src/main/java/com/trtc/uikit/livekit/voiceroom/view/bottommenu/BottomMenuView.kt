package com.trtc.uikit.livekit.voiceroom.view.bottommenu

import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup.LayoutParams.MATCH_PARENT
import android.widget.ImageView
import android.widget.RelativeLayout
import androidx.core.view.isVisible
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.LifecycleOwner
import androidx.lifecycle.LifecycleRegistry
import androidx.lifecycle.Observer
import androidx.lifecycle.lifecycleScope
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.trtc.tuikit.common.permission.PermissionCallback
import com.trtc.tuikit.common.system.ContextProvider
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.common.ErrorLocalized
import com.trtc.uikit.livekit.common.LiveKitLogger
import com.trtc.uikit.livekit.common.PermissionRequest
import com.trtc.uikit.livekit.common.TUIActionCallback
import com.trtc.uikit.livekit.component.barrage.BarrageInputView
import com.trtc.uikit.livekit.voiceroom.manager.VoiceRoomManager
import com.trtc.uikit.livekit.voiceroom.state.SeatState
import com.trtc.uikit.livekit.voiceroom.view.BasicView
import com.trtc.uikit.livekit.voiceroomcore.SeatGridView
import io.trtc.tuikit.atomicxcore.api.LiveSeatStore
import io.trtc.tuikit.atomicxcore.api.SeatInfo
import kotlinx.coroutines.Job
import kotlinx.coroutines.launch

class BottomMenuView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : BasicView(context, attrs, defStyleAttr), LifecycleOwner {

    private val logger = LiveKitLogger.getVoiceRoomLogger("BottomMenuView")

    private lateinit var barrageInputView: BarrageInputView
    private lateinit var microphoneContainer: View
    private lateinit var microphoneButton: ImageView

    private val linkStateObserver = Observer<SeatState.LinkStatus> { onLinkStateChanged(it) }
    private val microphoneMutedObserver = Observer<Boolean> { updateMicrophoneButton(it) }

    private val lifecycleRegistry = LifecycleRegistry(this)
    private val collectJobs = mutableListOf<Job>()
    private var seatStore: LiveSeatStore? = null

    init {
        lifecycleRegistry.currentState = Lifecycle.State.INITIALIZED
    }

    override val lifecycle: Lifecycle get() = lifecycleRegistry

    override fun initView() {
        LayoutInflater.from(context).inflate(R.layout.livekit_voiceroom_layout_bottom_menu, this, true)
        barrageInputView = findViewById(R.id.rl_barrage_button)
        microphoneContainer = findViewById(R.id.microphone_container)
        microphoneButton = findViewById(R.id.iv_microphone)
        microphoneButton.setOnClickListener { onMicrophoneButtonClick() }
    }

    override fun addObserver() {
        mSeatState.linkStatus.observeForever(linkStateObserver)
        mMediaState.isMicrophoneMuted.observeForever(microphoneMutedObserver)
        seatStore = LiveSeatStore.create(mRoomState.roomId)
        val job = lifecycleScope.launch {
            seatStore?.liveSeatState?.seatList?.collect { seatList ->
                onSeatMutedStateChanged(seatList)
            }
        }
        collectJobs.add(job)
        lifecycleRegistry.currentState = Lifecycle.State.STARTED
    }

    override fun removeObserver() {
        mSeatState.linkStatus.removeObserver(linkStateObserver)
        mMediaState.isMicrophoneMuted.removeObserver(microphoneMutedObserver)
        collectJobs.forEach { it.cancel() }
        collectJobs.clear()
        lifecycleRegistry.currentState = Lifecycle.State.DESTROYED
    }

    override fun init(voiceRoomManager: VoiceRoomManager, seatGridView: SeatGridView) {
        super.init(voiceRoomManager, seatGridView)
        val functionView = if (mUserState.selfInfo.userRole == TUIRoomDefine.Role.ROOM_OWNER) {
            AnchorFunctionView(context)
        } else {
            AudienceFunctionView(context)
        }.apply {
            init(voiceRoomManager, seatGridView)
        }

        findViewById<RelativeLayout>(R.id.function_container).apply {
            removeAllViews()
            addView(functionView, RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT))
        }
        barrageInputView.init(mRoomState.roomId)
    }

    private fun onLinkStateChanged(linkStatus: SeatState.LinkStatus) {
        microphoneContainer.isVisible = linkStatus == SeatState.LinkStatus.LINKING
    }

    private fun onMicrophoneButtonClick() {
        when {
            mMediaState.isMicrophoneOpened.value != true -> openLocalMicrophone()
            mMediaState.isMicrophoneMuted.value == true -> unMuteMicrophone()
            else -> muteMicrophone()
        }
    }

    private fun openLocalMicrophone() {
        if (mMediaState.hasMicrophonePermission.value == true) {
            openLocalMicrophoneInternal()
            return
        }
        PermissionRequest.requestMicrophonePermissions(
            ContextProvider.getApplicationContext(),
            object : PermissionCallback() {
                override fun onRequesting() {
                    logger.info("requestMicrophonePermissions")
                }

                override fun onGranted() {
                    logger.info("requestMicrophonePermissions:[onGranted]")
                    mMediaManager.updateMicrophonePermissionState(true)
                    openLocalMicrophoneInternal()
                }

                override fun onDenied() {
                    logger.warn("requestMicrophonePermissions:[onDenied]")
                }
            })
    }

    private fun openLocalMicrophoneInternal() {
        mSeatGridView.startMicrophone(
            TUIActionCallback(
                success = {
                    mMediaManager.updateMicrophoneOpenState(true)
                    unMuteMicrophone()
                },
                error = { errorCode, message ->
                    logger.error("startMicrophone failed, error: $errorCode, message: $message")
                    ErrorLocalized.onError(errorCode)
                }
            ))
    }

    private fun muteMicrophone() {
        mSeatGridView.muteMicrophone()
        mMediaManager.updateMicrophoneMuteState(true)
    }

    private fun unMuteMicrophone() {
        mSeatGridView.unmuteMicrophone(
            TUIActionCallback(
                success = {
                    mMediaManager.updateMicrophoneMuteState(false)
                },
                error = { errorCode, message ->
                    logger.error("unmuteMicrophone failed, error: $errorCode, message: $message")
                    ErrorLocalized.onError(errorCode)
                }
            ))
    }

    private fun updateMicrophoneButton(isMicrophoneMuted: Boolean) {
        microphoneButton.setImageResource(
            if (isMicrophoneMuted) R.drawable.livekit_ic_mic_closed
            else R.drawable.livekit_ic_mic_opened
        )
    }

    private fun onSeatMutedStateChanged(seatList: List<SeatInfo>) {
        val seatInfo = seatList.firstOrNull { it.userInfo.userID == TUIRoomEngine.getSelfInfo().userId }
        seatInfo?.let {
            if (!it.userInfo.allowOpenMicrophone) {
                mMediaManager.updateMicrophoneMuteState(true)
            }
        }
    }
}
