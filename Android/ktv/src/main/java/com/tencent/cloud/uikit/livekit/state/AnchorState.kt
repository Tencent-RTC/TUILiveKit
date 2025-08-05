package com.tencent.cloud.uikit.livekit.state

import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update

data class AnchorState(
    val roomName: StateFlow<String>,
    val isPublicVisible: StateFlow<Boolean>,
    val coverUrl: StateFlow<String>,
)

class AnchorViewStore {
    private val _roomName = MutableStateFlow("")
    private val _isPublicVisible = MutableStateFlow(true)
    private val _coverUrl =
        MutableStateFlow("https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png")

    private val _anchorState = AnchorState(
        _roomName.asStateFlow(),
        _isPublicVisible.asStateFlow(),
        _coverUrl.asStateFlow()
    )

    fun getAnchorState(): AnchorState {
        return _anchorState
    }

    fun setRoomName(name: String) {
        _roomName.update { name }
    }

    fun setIsPublicVisible(boolean: Boolean) {
        _isPublicVisible.update { boolean }
    }

    fun setCoverUrl(url: String) {
        _coverUrl.update { url }
    }
}