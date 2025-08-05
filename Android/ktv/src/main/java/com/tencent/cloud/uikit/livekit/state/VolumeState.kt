package com.tencent.cloud.uikit.livekit.state

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update

data class VolumeInfo(
    val userId: String = "",
    val volume: Int = 0,
)

class VolumeState private constructor() {

    private val _volumeList = MutableStateFlow<List<VolumeInfo>>(emptyList())
    val volumeList: StateFlow<List<VolumeInfo>> = _volumeList.asStateFlow()

    private val roomEngine: TUIRoomEngine = TUIRoomEngine.sharedInstance()
    private val roomObserver = object : TUIRoomObserver() {
        override fun onUserVoiceVolumeChanged(volumeMap: Map<String?, Int?>?) {
            val list = volumeMap?.mapNotNull { (userId, volume) ->
                if (userId != null && volume != null) {
                    VolumeInfo(userId, volume)
                } else {
                    null
                }
            } ?: emptyList()
            _volumeList.update { list }
        }
    }

    init {
        addObserver()
    }

    private fun addObserver() {
        roomEngine.addObserver(roomObserver)
    }

    private fun removeObserver() {
        roomEngine.removeObserver(roomObserver)
    }

    fun release() {
        removeObserver()
    }

    companion object {
        private const val TAG = "VolumeState"

        val shared by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
            VolumeState()
        }

        @JvmStatic
        fun getInstance(): VolumeState = shared
    }
}