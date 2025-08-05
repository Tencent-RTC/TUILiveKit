package com.tencent.cloud.uikit.livekit

import androidx.lifecycle.ViewModel
import com.tencent.cloud.uikit.livekit.karaoke.manager.KaraokeStore
import com.tencent.cloud.uikit.livekit.karaoke.state.KaraokeState
import com.tencent.cloud.uikit.livekit.songpicker.SongState
import com.tencent.cloud.uikit.livekit.songpicker.model.ContextInfo
import com.tencent.cloud.uikit.livekit.songpicker.service.SongPickerStore
import com.tencent.cloud.uikit.livekit.state.AnchorViewStore
import com.tencent.cloud.uikit.livekit.utils.Logger
import kotlinx.coroutines.flow.asStateFlow

/**
 * eg: use the ViewModel
 *
 * </br> in activity: val viewModel = ViewModelProvider(owner).get(roomId, KTVViewModel::class.java)
 * </br> in compose:  val viewModel = viewModel(KTVViewModel::class, key = roomId)
 */
class KTVViewModel() : ViewModel() {
    val anchorViewStore = AnchorViewStore()

    val songPickerStore = SongPickerStore()

    val songState = SongState(
        songPickerStore._musicSelectedList.asStateFlow(),
        songPickerStore._currentSong.asStateFlow()
    )

    val karaokeStore = KaraokeStore()

    val karaokeState = KaraokeState(
        karaokeStore._isAccompany.asStateFlow(),
        karaokeStore._currentPlayProgress.asStateFlow(),
        karaokeStore._currentPlayDuration.asStateFlow(),
        karaokeStore._playState.asStateFlow(),
        karaokeStore._standardPitchModels.asStateFlow(),
        karaokeStore._currentPitch.asStateFlow(),
        karaokeStore._playScore.asStateFlow(),
        karaokeStore._isMicrophoneMuted.asStateFlow(),
        karaokeStore._liveStatus.asStateFlow(),
    )

    fun setContextInfo(contextInfo: ContextInfo) {
        songPickerStore.setContextInfo(contextInfo)
    }

    fun cleanData() {
        LOGGER.info("cleanData")
        karaokeStore.cleanData()
    }

    companion object {
        val LOGGER: Logger = Logger.getKTVLogger("KTVViewModel")
    }

    override fun onCleared() {
        LOGGER.info("onCleared")
    }
}