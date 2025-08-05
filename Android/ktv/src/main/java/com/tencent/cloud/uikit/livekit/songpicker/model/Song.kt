package com.tencent.cloud.uikit.livekit.songpicker.model

class Song {
    var id: String = ""
    var name: String = ""
    var singers: List<String> = emptyList()
    var lrcUrl: String = ""
    var coverUrl: String = ""
    var originUrl: String = ""
    var accompanyUrl: String = ""
    var midiUrl: String = ""
    var status: Int = 0
    var userId: String = ""
    var performId: String = ""
    var playToken: String = ""
    var isSelected: Boolean = false
}