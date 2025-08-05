package com.tencent.cloud.uikit.livekit.songpicker.service

import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.songpicker.model.ContextInfo
import com.tencent.cloud.uikit.livekit.songpicker.model.Song
import com.tencent.cloud.uikit.livekit.songpicker.model.SongTag
import com.trtc.tuikit.common.system.ContextProvider
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update

data class ToastMessage(val message: String, val time: Long = System.currentTimeMillis())

class SongPickerStore() {
    val _currentSong = MutableStateFlow<Song>(Song())
    val _musicSelectedList = MutableStateFlow<MutableList<Song>>(mutableListOf())
    val _toastMessage = MutableStateFlow<ToastMessage>(ToastMessage(""))

    private var contextInfo = ContextInfo("", "", "")
    private var musicService: MusicService? = null
    private val musicServiceObserver = MusicServiceObserver()

    private val _musicTagList: MutableStateFlow<List<SongTag>> = MutableStateFlow(mutableListOf())
    val musicTagList: StateFlow<List<SongTag>> = _musicTagList.asStateFlow()

    private val _hasLoadMusicsByTag = MutableStateFlow<String>("")
    val hasLoadMusicsByTag: StateFlow<String> = _hasLoadMusicsByTag.asStateFlow()

    private val _currentRequestIndex = MutableStateFlow<Int>(0)
    val currentRequestIndex: StateFlow<Int> = _currentRequestIndex.asStateFlow()

    val musicInfoCache: MutableMap<String, List<Song>> = HashMap()

    fun setContextInfo(contextInfo: ContextInfo) {
        if (this.musicService != null) {
            return
        }
        this.contextInfo = contextInfo
        musicService = DefaultMusicService(contextInfo)
        musicService?.addObserver(musicServiceObserver)
        if (musicService is DefaultMusicService) {
            (musicService as DefaultMusicService).initPlayList()
        }
    }

    fun setCurrentRequestIndex(index: Int) {
        _currentRequestIndex.update { index }
    }

    fun updateMusicTagList() {
        musicService?.getMusicTagList(object : MusicService.ValueCallback<List<SongTag>> {
            override fun onSuccess(value: List<SongTag>) {
                _musicTagList.update { list ->
                    value
                }
                musicInfoCache.clear()
                value.forEach { tag ->
                    musicInfoCache.put(tag.id, emptyList())
                }
            }

            override fun onError(errorCode: Int, errorMessage: String) {

            }
        })
    }

    fun loadMusicsByTagId(tag: String) {
        musicService?.getMusicsByTagId(
            tag,
            "",
            object : MusicService.ValueCallback<List<Song>> {
                override fun onSuccess(value: List<Song>) {
                    musicInfoCache.put(tag, value)
                    _hasLoadMusicsByTag.update { t ->
                        tag
                    }
                }

                override fun onError(errorCode: Int, errorMessage: String) {

                }
            })
    }

    fun addMusicToPlaylist(musicInfo: Song) {
        musicService?.addMusicToPlaylist(musicInfo, object : MusicService.AddMusicCallback {
            override fun onStart(musicInfo: Song) {

            }

            override fun onProgress(musicInfo: Song, progress: Float) {

            }

            override fun onFinish(musicInfo: Song, errorCode: Int, errorMessage: String) {
                if (errorCode == -1) _toastMessage.update {
                    ToastMessage(getString(R.string.ktv_toast_room_owner_can_operate_it))
                }
            }
        })
    }

    fun deleteMusicFromPlaylist(musicInfo: Song) {
        musicService?.deleteMusicFromPlaylist(musicInfo, object : MusicService.ActionCallback {
            override fun onSuccess() {

            }

            override fun onError(errorCode: Int, errorMessage: String) {
                if (errorCode == -1) _toastMessage.update {
                    ToastMessage(getString(R.string.ktv_toast_room_owner_can_operate_it))
                }
            }
        })
    }

    fun topMusic(musicInfo: Song) {
        musicService?.topMusic(musicInfo, object : MusicService.ActionCallback {
            override fun onSuccess() {

            }

            override fun onError(errorCode: Int, errorMessage: String) {
                if (errorCode == -1) _toastMessage.update {
                    ToastMessage(getString(R.string.ktv_toast_room_owner_can_operate_it))
                }
            }
        })
    }

    fun getMusicsByKeywords(
        scrollToken: String,
        pageSize: Int,
        keyWords: String,
        callback: MusicService.ValueCallback<MusicService.MusicInfoPage>
    ) {
        musicService?.getMusicsByKeywords(scrollToken, pageSize, keyWords, callback)
    }

    private fun getString(resId: Int): String {
        return ContextProvider.getApplicationContext().getString(resId)
    }

    private inner class MusicServiceObserver : MusicService.Observer {
        override fun onMusicListChanged(musicInfoList: MutableList<Song>) {
            _musicSelectedList.update { musicInfoList }
            _currentSong.update { music ->
                if (_musicSelectedList.value.isEmpty()) Song() else _musicSelectedList.value.first()
            }
        }
    }
}