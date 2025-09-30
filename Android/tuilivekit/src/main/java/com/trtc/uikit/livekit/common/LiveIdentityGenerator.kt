package com.trtc.uikit.livekit.common

object LiveIdentityGenerator {
    fun generateId(id: String, type: RoomType) = type.prefix + id

    enum class RoomType(val prefix: String) {
        LIVE("live_"),
        VOICE("voice_")
    }
}