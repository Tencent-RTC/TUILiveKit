package com.tencent.cloud.uikit.livekit.songpicker.service

import com.google.gson.annotations.SerializedName

class KaraokeJsonData {
    var version: Int = 0
    var businessID: String? = null
    var platform: String? = null
    var data: Data? = null

    class Data {
        @SerializedName("room_id")
        var roomId: String? = null

        @SerializedName("instruction")
        var instruction: String? = null

        @SerializedName("content")
        var content: String? = null

        @SerializedName("music_id")
        var musicId: String? = null
    }
}
