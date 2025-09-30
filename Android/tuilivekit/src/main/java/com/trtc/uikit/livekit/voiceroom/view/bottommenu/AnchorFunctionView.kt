package com.trtc.uikit.livekit.voiceroom.view.bottommenu

import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.widget.TextView
import androidx.core.view.isVisible
import androidx.lifecycle.Observer
import com.trtc.uikit.livekit.R
import com.trtc.uikit.livekit.voiceroom.state.RoomState
import com.trtc.uikit.livekit.voiceroom.state.SeatState
import com.trtc.uikit.livekit.voiceroom.view.BasicView
import com.trtc.uikit.livekit.voiceroom.view.seatmanager.SeatManagerDialog
import com.trtc.uikit.livekit.voiceroom.view.settings.SettingsDialog
import io.trtc.tuikit.atomicx.karaoke.KaraokeControlView
import io.trtc.tuikit.atomicx.karaoke.KaraokeFloatingView

class AnchorFunctionView @JvmOverloads constructor(
    context: Context,
    attrs: AttributeSet? = null,
    defStyleAttr: Int = 0
) : BasicView(context, attrs, defStyleAttr) {

    private lateinit var seatApplicationCountText: TextView
    private var settingsDialog: SettingsDialog? = null
    private var seatManagerDialog: SeatManagerDialog? = null

    private val seatApplicationListObserver = Observer<LinkedHashSet<SeatState.SeatApplication>> {
        updateSeatApplicationCountText(it)
    }

    override fun initView() {
        LayoutInflater.from(context).inflate(R.layout.livekit_voiceroom_anchor_function, this, true)
        seatApplicationCountText = findViewById(R.id.application_count)
        findViewById<View>(R.id.iv_settings).setOnClickListener { showSettingsPanel() }
        findViewById<View>(R.id.iv_song_request).setOnClickListener { showSongRequestPanel() }
        findViewById<View>(R.id.iv_seat_management).setOnClickListener { showSeatManagementPanel() }
    }

    override fun addObserver() {
        mSeatState.seatApplicationList.observeForever(seatApplicationListObserver)
    }

    override fun removeObserver() {
        mSeatState.seatApplicationList.removeObserver(seatApplicationListObserver)
    }

    private fun showSettingsPanel() {
        settingsDialog = settingsDialog ?: SettingsDialog(context, mVoiceRoomManager)
        settingsDialog?.show()
    }

    private fun showSongRequestPanel() {
        when (mRoomState.layoutType.value) {
            RoomState.LayoutType.KTVRoom -> KaraokeControlView(context).apply {
                init(mRoomState.roomId, mUserState.selfInfo.userId == mRoomState.ownerInfo.userId)
                showSongRequestPanel()
            }

            else -> KaraokeFloatingView(context).apply {
                init(mRoomState.roomId, mUserState.selfInfo.userId == mRoomState.ownerInfo.userId)
                showSongRequestPanel()
            }
        }
    }

    private fun showSeatManagementPanel() {
        seatManagerDialog = seatManagerDialog ?: SeatManagerDialog(context, mVoiceRoomManager, mSeatGridView)
        seatManagerDialog?.show()
    }

    private fun updateSeatApplicationCountText(list: LinkedHashSet<SeatState.SeatApplication>) {
        seatApplicationCountText.apply {
            isVisible = list.isNotEmpty()
            text = list.size.toString()
        }
    }
}
