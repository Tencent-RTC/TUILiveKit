package com.tencent.cloud.uikit.livekit.songpicker.service

import androidx.core.content.ContextCompat
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomMetadataCallback
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver
import com.tencent.cloud.uikit.livekit.songpicker.model.ContextInfo
import com.tencent.cloud.uikit.livekit.songpicker.model.SimpleSong
import com.tencent.cloud.uikit.livekit.songpicker.model.Song
import com.tencent.cloud.uikit.livekit.songpicker.model.SongTag
import com.tencent.cloud.uikit.livekit.songpicker.service.MusicService.ActionCallback
import com.tencent.cloud.uikit.livekit.utils.Logger
import com.trtc.tuikit.common.system.ContextProvider

private val LOGGER = Logger.getKTVLogger("DefaultMusicService")

private const val KEY_SONG_PLAY_LIST = "SongPlayList"

private const val DEFAULT_COVER_URL =
    "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar2.png"

class DefaultMusicService(private val contextInfo: ContextInfo) : MusicService {
    val musicLibraryList: MutableList<Song> = mutableListOf()
    val musicSelectedList: MutableList<Song> = mutableListOf()
    val observerList: MutableList<MusicService.Observer> = mutableListOf()
    val engineObserver = object : TUIRoomObserver() {
        override fun onRoomMetadataChanged(key: String?, value: String?) {
            LOGGER.info("onRoomMetadataChanged, key:${key}, value:${value}")
            if (key == KEY_SONG_PLAY_LIST && value != null) {
                val musicList = parsePlayListJson(value)
                notifyLocalMusicListChange(musicList)
            }
        }
    }

    init {
        TUIRoomEngine.sharedInstance().addObserver(engineObserver)
        initMusicLibrary()
    }

    fun unInit() {
        LOGGER.info("unInit")
        TUIRoomEngine.sharedInstance().removeObserver(engineObserver)
    }

    fun isOwner(): Boolean {
        return contextInfo.ownerId == contextInfo.selfId
    }

    fun initMusicLibrary() {
        val context = ContextProvider.getApplicationContext()
        val localPath =
            ContextCompat.getExternalFilesDirs(context, null)[0].getAbsolutePath() + "/"
        val list = mutableListOf<Song>()
        list.add(Song().apply {
            id = "music_id"
            name = "music_name"
            singers = mutableListOf<String>("music_singers")
            coverUrl = DEFAULT_COVER_URL
            lrcUrl = localPath + "local_music_lrc.vtt"
            performId = "music_performId"
            originUrl = localPath + "local_music_origin.mp3"
            accompanyUrl = localPath + "local_music_accompany.mp3"
        })
        musicLibraryList.addAll(list)
    }

    fun initPlayList() {
        getPlaylist(object : MusicService.ValueCallback<List<Song>> {
            override fun onSuccess(value: List<Song>) {
                notifyLocalMusicListChange(value)
            }

            override fun onError(errorCode: Int, errorMessage: String) {

            }
        })
    }

    private fun parsePlayListJson(value: String): List<Song> {
        val list = Gson().fromJson<List<SimpleSong>>(
            value,
            object : TypeToken<List<SimpleSong>>() {}.type
        )
        val musicList = mutableListOf<Song>()
        list.forEach { music ->
            val song = musicLibraryList.firstOrNull { music.musicId == it.id }
            if (song != null) {
                musicList.add(song)
            }
        }
        return musicList
    }

    override fun addObserver(observer: MusicService.Observer) {
        if (!observerList.contains(observer)) {
            observerList.add(observer)
        }
    }

    override fun destroyService() {
        observerList.clear()
        unInit()
    }

    override fun getMusicTagList(callback: MusicService.ValueCallback<List<SongTag>>?) {
        val musicTagList = mutableListOf<SongTag>()
        musicTagList.add(SongTag("-10005", "local_demo_music"))
        callback?.onSuccess(musicTagList)
    }

    override fun getMusicsByTagId(
        tagId: String,
        scrollToken: String,
        callback: MusicService.ValueCallback<List<Song>>?
    ) {
        callback?.onSuccess(musicLibraryList)
    }

    override fun addMusicToPlaylist(
        musicInfo: Song,
        callback: MusicService.AddMusicCallback?
    ) {
        LOGGER.info("addMusicToPlaylist:Song(id=${musicInfo.id}, name=${musicInfo.name})")
        if (!isOwner()) {
            callback?.onFinish(Song(), -1, "not support option")
            return
        }
        if (musicSelectedList.firstOrNull { it.id == musicInfo.id } != null) {
            LOGGER.info("already add song(name=${musicInfo.name})")
            callback?.onProgress(musicInfo, 100F)
            callback?.onFinish(musicInfo, 0, "")
            return
        }
        musicInfo.isSelected = true
        musicInfo.userId = contextInfo.selfId
        val musicList = musicSelectedList.toMutableList().apply { add(musicInfo) }
        callback?.onStart(musicInfo)
        callback?.onProgress(musicInfo, 50F)
        updateMusicList(musicList, object : TUIRoomDefine.ActionCallback {
            override fun onSuccess() {
                callback?.onProgress(musicInfo, 100F)
                callback?.onFinish(musicInfo, 0, "")
            }

            override fun onError(error: TUICommonDefine.Error?, message: String?) {
                callback?.onFinish(musicInfo, error?.value ?: -1, message.toString())
            }
        })
    }

    override fun deleteMusicFromPlaylist(
        musicInfo: Song, callback: ActionCallback?
    ) {
        LOGGER.info("deleteMusicFromPlaylist:Song(id=${musicInfo.id}, name=${musicInfo.name})")
        if (!isOwner()) {
            callback?.onError(-1, "not support option")
            return
        }
        if (musicSelectedList.firstOrNull { it.id == musicInfo.id } == null) {
            LOGGER.info("not found song(name=${musicInfo.name}) to delete")
            callback?.onSuccess()
            return
        }
        val musicList = musicSelectedList.toMutableList().apply {
            val song = musicSelectedList.firstOrNull { it.id == musicInfo.id }
            if (song != null) {
                musicInfo.isSelected = false
                remove(song)
            }
        }
        updateMusicList(musicList, object : TUIRoomDefine.ActionCallback {
            override fun onSuccess() {
                callback?.onSuccess()
            }

            override fun onError(error: TUICommonDefine.Error?, message: String?) {
                callback?.onError(error?.value ?: -1, message.toString())
            }
        })
    }

    override fun topMusic(musicInfo: Song, callback: ActionCallback?) {
        LOGGER.info("topMusic:Song(id=${musicInfo.id}, name=${musicInfo.name})")
        if (!isOwner()) {
            callback?.onError(-1, "not support option")
            return
        }
        val musicList = musicSelectedList.toMutableList().apply {
            var index = indexOfFirst { it.id == musicInfo.id }
            if (index > 1) {
                var item = get(index)
                remove(item)
                add(1, item)
            }
        }
        updateMusicList(musicList, object : TUIRoomDefine.ActionCallback {
            override fun onSuccess() {
                callback?.onSuccess()
            }

            override fun onError(error: TUICommonDefine.Error?, message: String?) {
                callback?.onError(error?.value ?: -1, message.toString())
            }
        })
    }

    override fun getMusicsByKeywords(
        scrollToken: String,
        pageSize: Int,
        keyWords: String,
        callback: MusicService.ValueCallback<MusicService.MusicInfoPage>?
    ) {
        if (keyWords.isEmpty()) {
            callback?.onError(-1, "keyWords is empty!")
            return
        }
        val searchResultList =
            musicLibraryList.filter { it ->
                it.name.contains(keyWords) || it.singers.contains(
                    keyWords
                )
            }
        val page = MusicService.MusicInfoPage()
        page.musicList = searchResultList
        page.scrollToken = scrollToken
        callback?.onSuccess(page)
    }

    override fun clearPlaylistByUserId(
        userId: String, callback: ActionCallback?
    ) {
        LOGGER.error("Not yet implemented")
    }

    override fun switchMusicFromPlaylist(
        musicInfo: Song, callback: ActionCallback?
    ) {
        LOGGER.info("switchMusicFromPlaylist:Song(id=${musicInfo.id}, name=${musicInfo.name})")
        deleteMusicFromPlaylist(musicInfo, callback)
    }

    override fun completePlaying(musicInfo: Song, callback: ActionCallback?) {
        deleteMusicFromPlaylist(musicInfo, callback)
    }

    override fun getPlaylist(callback: MusicService.ValueCallback<List<Song>>?) {
        TUIRoomEngine.sharedInstance()
            .getRoomMetadata(listOf(KEY_SONG_PLAY_LIST), object : GetRoomMetadataCallback {
                override fun onSuccess(map: java.util.HashMap<String?, String?>?) {
                    if (map == null) {
                        callback?.onSuccess(emptyList())
                        return
                    }
                    val playListJson = map.get(KEY_SONG_PLAY_LIST)
                    LOGGER.info("getRoomMetadata, key:${KEY_SONG_PLAY_LIST}, value:${playListJson}")
                    val musicList =
                        if (playListJson == null) emptyList() else parsePlayListJson(playListJson)
                    callback?.onSuccess(musicList)
                }

                override fun onError(error: TUICommonDefine.Error?, message: String?) {
                    LOGGER.error("getRoomMetadata, code:${error}, desc:${message}")
                    callback?.onError(error?.value ?: -1, message.toString())
                }
            })
    }

    private fun notifyLocalMusicListChange(musicList: List<Song>) {
        musicSelectedList.clear()
        musicSelectedList.addAll(musicList)
        observerList.forEach {
            it.onMusicListChanged(musicSelectedList.toMutableList())
        }
    }

    private fun updateMusicList(
        musicList: List<Song>, callback: TUIRoomDefine.ActionCallback?
    ) {
        val simpleSongList = musicList.map { music ->
            SimpleSong().apply {
                musicId = music.id
                name = music.name
            }
        }
        val map = HashMap<String, String>()
        val data = Gson().toJson(simpleSongList)
        map.put(KEY_SONG_PLAY_LIST, data)
        TUIRoomEngine.sharedInstance()
            .setRoomMetadataByAdmin(map, object : TUIRoomDefine.ActionCallback {
                override fun onSuccess() {
                    callback?.onSuccess()
                }

                override fun onError(error: TUICommonDefine.Error?, message: String?) {
                    LOGGER.error("setRoomMetadataByAdmin, code:${error}, desc:${message}")
                    callback?.onError(error, message)
                }
            })
    }
}