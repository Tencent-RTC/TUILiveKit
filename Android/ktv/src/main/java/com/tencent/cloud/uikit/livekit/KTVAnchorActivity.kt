package com.tencent.cloud.uikit.livekit

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.lifecycle.ViewModelProvider
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.uikit.livekit.songpicker.MusicLibrary
import com.tencent.cloud.uikit.livekit.utils.ActivityUtils
import com.tencent.cloud.uikit.livekit.view.anchor.AnchorView

class KTVAnchorActivity : ComponentActivity() {
    val mLiveIdExtraKey: String = "intent_key_room_id"
    lateinit var viewModel: KTVViewModel

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        ActivityUtils.setActivityFullScreen(this, false)
        val liveId = intent.getStringExtra(mLiveIdExtraKey).toString()
        viewModel = ViewModelProvider(this)[liveId, KTVViewModel::class.java]
        viewModel.anchorViewStore.setRoomName(TUIRoomEngine.getSelfInfo().userName)
        MusicLibrary.shared.copyLocalMusic(this)
        setContent {
            if (!liveId.isEmpty()) {
                AnchorView(
                    liveId = liveId,
                    onStartLive = { startLiveFlow(liveId = liveId) }
                )
            }
        }
    }

    override fun onDestroy() {
        super.onDestroy()
        viewModel.cleanData()
    }

    private fun startLiveFlow(
        liveId: String = ""
    ) {
        val liveInfo = TUILiveListManager.LiveInfo()
        liveInfo.roomId = liveId
        liveInfo.name = viewModel.anchorViewStore.getAnchorState().roomName.value
        liveInfo.isPublicVisible = viewModel.anchorViewStore.getAnchorState().isPublicVisible.value
        liveInfo.coverUrl = viewModel.anchorViewStore.getAnchorState().coverUrl.value
        liveInfo.seatMode = TUIRoomDefine.SeatMode.FREE_TO_TAKE
        liveInfo.maxSeatCount = 4
        viewModel.karaokeStore.createLive(liveInfo)
    }
}