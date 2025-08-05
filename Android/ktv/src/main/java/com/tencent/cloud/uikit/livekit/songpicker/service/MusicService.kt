package com.tencent.cloud.uikit.livekit.songpicker.service;

import com.tencent.cloud.uikit.livekit.songpicker.model.Song
import com.tencent.cloud.uikit.livekit.songpicker.model.SongTag

interface MusicService {

    fun addObserver(observer: Observer)

    fun destroyService()

    fun getMusicTagList(callback: ValueCallback<List<SongTag>>?)

    fun getMusicsByTagId(
        tagId: String,
        scrollToken: String,
        callback: ValueCallback<List<Song>>?
    )

    fun addMusicToPlaylist(musicInfo: Song, callback: AddMusicCallback?)

    fun deleteMusicFromPlaylist(musicInfo: Song, callback: ActionCallback?)

    fun clearPlaylistByUserId(userId: String, callback: ActionCallback?)

    fun topMusic(musicInfo: Song, callback: ActionCallback?)

    fun switchMusicFromPlaylist(musicInfo: Song, callback: ActionCallback?)

    fun completePlaying(musicInfo: Song, callback: ActionCallback?)

    fun getMusicsByKeywords(
        scrollToken: String,
        pageSize: Int,
        keyWords: String,
        callback: ValueCallback<MusicInfoPage>?
    )

    fun getPlaylist(callback: ValueCallback<List<Song>>?)

    interface ActionCallback {
        fun onSuccess()
        fun onError(errorCode: Int, errorMessage: String)
    }

    interface ValueCallback<T> {
        fun onSuccess(value: T)
        fun onError(errorCode: Int, errorMessage: String)
    }

    interface AddMusicCallback {
        fun onStart(musicInfo: Song)
        fun onProgress(musicInfo: Song, progress: Float)
        fun onFinish(musicInfo: Song, errorCode: Int, errorMessage: String)
    }

    interface Observer {
        fun onMusicListChanged(musicInfoList: MutableList<Song>)
    }

    class MusicInfoPage(var musicList: List<Song> = emptyList(), var scrollToken: String = "")
}
