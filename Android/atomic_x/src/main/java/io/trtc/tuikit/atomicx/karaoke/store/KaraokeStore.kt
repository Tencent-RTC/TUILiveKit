package io.trtc.tuikit.atomicx.karaoke.store


import android.content.Context
import android.os.Handler
import android.os.Looper
import android.util.Log
import androidx.core.content.ContextCompat
import androidx.lifecycle.LiveData
import androidx.lifecycle.MutableLiveData
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.GetRoomMetadataCallback
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.RoomDismissedReason
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver
import com.tencent.trtc.TRTCCloud
import com.tencent.trtc.TRTCCloudDef.TRTCParams
import com.tencent.trtc.TXChorusMusicPlayer
import com.tencent.trtc.TXChorusMusicPlayer.TXChorusExternalMusicParams
import com.tencent.trtc.TXChorusMusicPlayer.TXChorusRole
import io.trtc.tuikit.atomicx.R
import io.trtc.tuikit.atomicx.karaoke.store.utils.GenerateTestUserSig
import io.trtc.tuikit.atomicx.karaoke.store.utils.LyricsFileReader
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicInfo
import io.trtc.tuikit.atomicx.karaoke.store.utils.MusicSelection
import io.trtc.tuikit.atomicx.karaoke.store.utils.PlaybackState
import java.io.File
import java.io.FileOutputStream
import java.nio.ByteBuffer

class KaraokeStore private constructor(private val context: Context) {
    companion object {
        @Volatile
        private var instance: KaraokeStore? = null

        @JvmStatic
        fun getInstance(context: Context): KaraokeStore {
            return instance ?: synchronized(this) {
                instance ?: KaraokeStore(context.applicationContext).also { instance = it }
            }
        }

        @JvmStatic
        fun destroyInstance() {
            instance?.destroy()
            instance = null
        }
    }

    var isAwaitingScoreDisplay = true
    private var chorusPlayer: TXChorusMusicPlayer? = null
    private val trtcCloud: TRTCCloud = TUIRoomEngine.sharedInstance().trtcCloud
    private val gson = Gson()
    private val mainHandler = Handler(Looper.getMainLooper())
    private val _currentPlayingSong = MutableLiveData<MusicSelection>()
    val currentPlayingSong: LiveData<MusicSelection> get() = _currentPlayingSong
    private val _isRoomOwner = MutableLiveData(false)
    val isRoomOwner: LiveData<Boolean> get() = _isRoomOwner
    private val _pendingSongAdds = MutableLiveData<Set<String>>(emptySet())
    val pendingSongAdds: LiveData<Set<String>> get() = _pendingSongAdds
    private val _songCatalog = MutableLiveData<List<MusicInfo>>(emptyList())
    val songCatalog: LiveData<List<MusicInfo>> get() = _songCatalog
    private val _songQueue = MutableLiveData<List<MusicSelection>>(emptyList())
    val songQueue: LiveData<List<MusicSelection>> get() = _songQueue
    private val _currentAudioTrack =
        MutableLiveData(TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusOriginalSong)
    val currentTrack: LiveData<TXChorusMusicPlayer.TXChorusMusicTrack> get() = _currentAudioTrack
    private val _playbackProgressMs = MutableLiveData(0L)
    val playbackProgressMs: LiveData<Long> get() = _playbackProgressMs
    private val _songDurationMs = MutableLiveData(0L)
    val songDurationMs: LiveData<Long> get() = _songDurationMs
    private val _playbackState = MutableLiveData(PlaybackState.IDLE)
    val playbackState: LiveData<PlaybackState> get() = _playbackState
    private val _isDisplayFloatView = MutableLiveData(true)
    val isDisplayFloatView: LiveData<Boolean> get() = _isDisplayFloatView
    private val _songPitchData =
        MutableLiveData<List<TXChorusMusicPlayer.TXReferencePitch>>(emptyList())
    val songPitchData: LiveData<List<TXChorusMusicPlayer.TXReferencePitch>> get() = _songPitchData
    private val _songLyrics = MutableLiveData<List<TXChorusMusicPlayer.TXLyricLine>>(emptyList())
    val songLyrics: LiveData<List<TXChorusMusicPlayer.TXLyricLine>> get() = _songLyrics
    private val _currentScore = MutableLiveData(0)
    val currentScore: LiveData<Int> get() = _currentScore
    private val _averageScore = MutableLiveData(100.0F)
    val averageScore: LiveData<Float> get() = _averageScore
    private val _publishVolume = MutableLiveData(60)
    val publishVolume: LiveData<Int> get() = _publishVolume
    private val _playoutVolume = MutableLiveData(95)
    val playoutVolume: LiveData<Int> get() = _playoutVolume
    private val _songPitch = MutableLiveData(0.0F)
    val songPitch: LiveData<Float> get() = _songPitch
    private val _isScoringEnabled = MutableLiveData(false)
    val isScoringEnabled: LiveData<Boolean> get() = _isScoringEnabled
    private val _isRoomDismissed = MutableLiveData(false)
    val isRoomDismissed: LiveData<Boolean> get() = _isRoomDismissed
    private var _isManualStop = false
    val KEY_ENABLE_REQUEST_MUSIC = "EnableRequestMusic"
    val KEY_PLAY_QUEUE = "SongPlayList"
    val KEY_ENABLE_SCORE = "EnableScore"
    val KEY_LOCAL_DEMO = "local_demo"

    fun init(roomId: String, isOwner: Boolean) {
        if (chorusPlayer != null) return
        _isRoomOwner.value = isOwner
        setupChorusPlayer(roomId)
        copyAllAssetsToStorage()
        fetchRoomMetadata()
        if (isOwner) {
            initializeAudioSettings()
        }
    }

    fun destroy() {
        stopPlayback()
        updateRemoteSongQueue(emptyList())
        _isRoomDismissed.value = true
        _songQueue.value = emptyList()
        _pendingSongAdds.value = emptySet()
        _currentPlayingSong.value = MusicSelection()
        _playbackProgressMs.value = 0L
        _songLyrics.value = emptyList()
        _songPitchData.value = emptyList()
        _currentScore.value = 0
        _playbackState.value = PlaybackState.IDLE
        _isManualStop = true
        _averageScore.value = 100.0F
        _currentAudioTrack.value = TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusOriginalSong
        TUIRoomEngine.sharedInstance().removeObserver(roomEngineObserver)
        chorusPlayer?.destroy()
        chorusPlayer = null
    }

    fun enableRequestMusic(enable: Boolean) {
        if (_isDisplayFloatView.value != enable) {
            val metadata = hashMapOf(KEY_ENABLE_REQUEST_MUSIC to enable.toString())
            TUIRoomEngine.sharedInstance().setRoomMetadataByAdmin(metadata, null)
            if (!enable) {
                updateRemoteSongQueue(emptyList())
                _songQueue.value = emptyList()
                _pendingSongAdds.value = emptySet()
                _currentPlayingSong.value = MusicSelection()
                _playbackProgressMs.value = 0L
                _songLyrics.value = emptyList()
                _songPitchData.value = emptyList()
                _currentScore.value = 0
                _playbackState.value = PlaybackState.IDLE
                _averageScore.value = 100.0F
                _currentAudioTrack.value =
                    TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusOriginalSong
                _isManualStop = true
                stopPlayback()
            }
        }
    }

    fun loadLocalDemoSong(musicId: String) {
        val musicInfo = findSongInCatalog(musicId) ?: return
        val params = TXChorusExternalMusicParams().apply {
            this.musicId = musicInfo.musicId
            musicUrl = musicInfo.originalUrl
            accompanyUrl = musicInfo.accompanyUrl
            encryptBlockLength = 0
            isEncrypted = false
        }
        chorusPlayer?.loadExternalMusic(params)
    }

    fun setChorusRole(roomId: String, chorusRole: TXChorusRole) {
        val params = TRTCParams().apply {
            sdkAppId = GenerateTestUserSig.SDKAPPID
            strRoomId = roomId
            userId = "${roomId}_bgm"
            userSig = GenerateTestUserSig.genTestUserSig(userId)
        }
        chorusPlayer?.setChorusRole(chorusRole, params)
    }

    fun startPlayback() {
        if (isRoomOwner.value == true) {
            chorusPlayer?.start()
            switchMusicTrack(TXChorusMusicPlayer.TXChorusMusicTrack.TXChorusOriginalSong)
        }
    }

    fun stopPlayback() {
        if (isRoomOwner.value == true) {
            chorusPlayer?.stop()
        }
    }

    fun pausePlayback() {
        if (isRoomOwner.value == true) {
            chorusPlayer?.pause()
        }
    }

    fun resumePlayback() {
        if (isRoomOwner.value == true) {
            chorusPlayer?.resume()
        }
    }

    fun switchMusicTrack(trackType: TXChorusMusicPlayer.TXChorusMusicTrack) {
        chorusPlayer?.switchMusicTrack(trackType)
        _currentAudioTrack.value = trackType
    }

    fun setPlayoutVolume(volume: Int?) {
        volume?.let {
            chorusPlayer?.setPlayoutVolume(it)
            _playoutVolume.value = it
        }
    }

    fun setPublishVolume(volume: Int?) {
        volume?.let {
            chorusPlayer?.setPublishVolume(it)
            _publishVolume.value = it
        }
    }

    fun setMusicPitch(pitch: Float?) {
        pitch?.let {
            chorusPlayer?.setMusicPitch(it)
            _songPitch.value = it
        }
    }

    fun addSongToQueue(music: MusicSelection) {
        val musicId = music.musicId
        val isAlreadyOrdered = _songQueue.value.orEmpty().any { it.musicId == musicId }
        val isAlreadyPending = _pendingSongAdds.value.orEmpty().contains(musicId)

        if (isAlreadyOrdered || isAlreadyPending) {
            return
        }

        val newPendingSet = _pendingSongAdds.value.orEmpty().toMutableSet()
        newPendingSet.add(musicId)
        _pendingSongAdds.postValue(newPendingSet)

        if (_songQueue.value.orEmpty().isEmpty()) {
            loadLocalDemoSong(musicId)
        }

        val updatedList = _songQueue.value.orEmpty() + music
        updateRemoteSongQueue(updatedList, musicId)
    }

    fun removeSongFromQueue(music: MusicSelection) {
        val updatedList = _songQueue.value.orEmpty().filter { it.musicId != music.musicId }
        updateRemoteSongQueue(updatedList)
    }

    fun playSongNext(music: MusicSelection) {
        val currentList = _songQueue.value.orEmpty().toMutableList()
        currentList.removeAll { it.musicId == music.musicId }
        val insertIndex = if (currentList.isNotEmpty()) 1 else 0
        currentList.add(insertIndex, music)
        updateRemoteSongQueue(currentList)
    }

    fun playNextSongInQueue() {
        if (_isRoomOwner.value == false) return
        if (_playbackState.value != PlaybackState.STOP && _playbackState.value != PlaybackState.IDLE) {
            stopPlayback()
        }
        val currentQueue = _songQueue.value.orEmpty()
        val updatedQueue = currentQueue.drop(1)
        updateRemoteSongQueue(updatedQueue)
        updatedQueue.firstOrNull()?.let {
            loadLocalDemoSong(it.musicId)
        }
    }

    fun setIsDisplayScoreView(isDisplay: Boolean) {
        isAwaitingScoreDisplay = isDisplay
    }

    fun updateRemoteSongQueue(list: List<MusicSelection>, musicIdBeingAdded: String? = null) {
        if (isRoomOwner.value != true) {
            if (musicIdBeingAdded != null) {
                val currentPending = _pendingSongAdds.value.orEmpty().toMutableSet()
                if (currentPending.remove(musicIdBeingAdded)) {
                    _pendingSongAdds.postValue(currentPending)
                }
            }
            return
        }

        val metadata = hashMapOf(KEY_PLAY_QUEUE to gson.toJson(list))
        TUIRoomEngine.sharedInstance()
            .setRoomMetadataByAdmin(metadata, object : TUIRoomDefine.ActionCallback {
                override fun onSuccess() {
                }

                override fun onError(
                    error: TUICommonDefine.Error?,
                    message: String?,
                ) {
                    if (musicIdBeingAdded != null) {
                        val currentPending = _pendingSongAdds.value.orEmpty().toMutableSet()
                        if (currentPending.remove(musicIdBeingAdded)) {
                            _pendingSongAdds.postValue(currentPending)
                        }
                    }
                }
            })
    }

    fun updatePlaybackStatus(state: PlaybackState) {
        if (_playbackState.value == PlaybackState.STOP) {
            _playbackState.value = state
        }
    }

    fun setScoringEnabled(enable: Boolean) {
        _isScoringEnabled.value = enable
        val metadata = hashMapOf(KEY_ENABLE_SCORE to enable.toString())
        TUIRoomEngine.sharedInstance().setRoomMetadataByAdmin(metadata, null)
    }

    fun updateSongCatalog(selectedList: List<MusicInfo>) {
        _songCatalog.value = selectedList
    }

    private fun setupChorusPlayer(roomId: String) {
        TUIRoomEngine.sharedInstance().addObserver(roomEngineObserver)
        chorusPlayer = TXChorusMusicPlayer.create(trtcCloud, roomId, chorusMusicObserver)
        val role = if (_isRoomOwner.value == true) {
            TXChorusRole.TXChorusRoleLeadSinger
        } else {
            TXChorusRole.TXChorusRoleAudience
        }
        setChorusRole(roomId, role)
    }

    private fun initializeAudioSettings() {
        setPlayoutVolume(playoutVolume.value)
        setPublishVolume(publishVolume.value)
        setMusicPitch(songPitch.value)
        applyDefaultAudioEffects()
    }

    private fun fetchRoomMetadata() {
        TUIRoomEngine.sharedInstance().getRoomMetadata(
            listOf(KEY_PLAY_QUEUE, KEY_ENABLE_SCORE, KEY_ENABLE_REQUEST_MUSIC),
            object : GetRoomMetadataCallback {
                override fun onSuccess(map: HashMap<String?, String?>?) {
                    map?.let {
                        if (isRoomOwner.value == true) {
                            if (_songQueue.value?.isNotEmpty() == true) {
                                updateRemoteSongQueue(emptyList())
                            }
                            if (_isScoringEnabled.value == true) {
                                setScoringEnabled(false)
                            }
                        } else {
                            _songQueue.value = parsePlayQueue(it)
                            _isScoringEnabled.value = it[KEY_ENABLE_SCORE]?.toBoolean() ?: false
                            _isDisplayFloatView.value =
                                it[KEY_ENABLE_REQUEST_MUSIC]?.toBoolean() ?: true
                        }
                    }
                }

                override fun onError(error: TUICommonDefine.Error?, message: String?) {}
            })
    }

    private fun findSongInCatalog(musicId: String): MusicInfo? {
        return songCatalog.value?.firstOrNull { it.musicId == musicId }
    }

    private fun findSongLyricPath(musicId: String): String {
        return _songCatalog.value?.find { it.musicId == musicId }?.lyricUrl ?: ""
    }

    private val roomEngineObserver: TUIRoomObserver = object : TUIRoomObserver() {
        override fun onRoomDismissed(
            roomId: String?,
            reason: RoomDismissedReason?,
        ) {
            destroy()
        }

        override fun onRoomMetadataChanged(key: String?, value: String?) {
            when (key) {
                KEY_PLAY_QUEUE -> {
                    try {
                        val type = object : TypeToken<List<MusicSelection>>() {}.type
                        val newList: List<MusicSelection> = gson.fromJson(value ?: "[]", type)

                        _songQueue.value = newList
                        _currentPlayingSong.value = updateCurrentPlayingSong()

                        val currentPending = _pendingSongAdds.value.orEmpty()
                        if (currentPending.isNotEmpty()) {
                            val orderedIds = newList.map { it.musicId }.toSet()
                            val newPending =
                                currentPending.filterNot { orderedIds.contains(it) }.toSet()
                            if (newPending.size != currentPending.size) {
                                _pendingSongAdds.value = newPending
                            }
                        }
                    } catch (e: Exception) {
                        e.printStackTrace()
                    }
                }

                KEY_ENABLE_SCORE -> {
                    _isScoringEnabled.value = value?.toBoolean() ?: true
                }

                KEY_ENABLE_REQUEST_MUSIC -> {
                    _isDisplayFloatView.value = value?.toBoolean() ?: true
                }
            }
        }
    }

    private fun updateCurrentPlayingSong(): MusicSelection {
        val currentMusic =
            _songQueue.value?.find { it.musicId == currentPlayingSong.value?.musicId }
                ?: MusicSelection()
        return currentMusic
    }

    private val chorusMusicObserver: TXChorusMusicPlayer.ITXChorusPlayerListener =
        object : TXChorusMusicPlayer.ITXChorusPlayerListener {
            override fun onChorusMusicLoadSucceed(
                musicId: String,
                lyricList: List<TXChorusMusicPlayer.TXLyricLine>,
                pitchList: List<TXChorusMusicPlayer.TXReferencePitch>,
            ) {
                if (musicId.startsWith(KEY_LOCAL_DEMO)) {
                    val musicPathTest = findSongLyricPath(musicId)
                    _songLyrics.value = LyricsFileReader().parseLyricInfo(musicPathTest)
                } else {
                    _songLyrics.value = lyricList
                    _songPitchData.value = pitchList
                }
                startPlayback()
                _currentPlayingSong.value = MusicSelection(musicId = musicId)
            }

            override fun onChorusError(error: TXChorusMusicPlayer.TXChorusError, errMsg: String) {
                Log.e("KaraokeStore", "onChorusError, error is: $error, errMsg: $errMsg")
            }

            override fun onNetworkQualityUpdated(userId: Int, upQuality: Int, downQuality: Int) {}

            override fun onChorusRequireLoadMusic(musicId: String) {
                loadLocalDemoSong(musicId)
            }

            override fun onChorusMusicLoadProgress(musicId: String, progress: Float) {}

            override fun onChorusStarted() {
                _playbackState.value = PlaybackState.START
                isAwaitingScoreDisplay = true
                if (isRoomOwner.value == true) {
                    enableReverb(true)
                }
            }

            override fun onChorusPaused() {
                if (_isRoomOwner.value == false && _songQueue.value.orEmpty().isEmpty()) {
                    return
                }
                _playbackState.value = PlaybackState.PAUSE
            }

            override fun onChorusResumed() {
                _playbackState.value = PlaybackState.RESUME
            }

            override fun onChorusStopped() {
                if (_isManualStop) {
                    _isManualStop = false
                    _playbackState.value = PlaybackState.IDLE
                    return
                }
                if (_isRoomOwner.value == false && _songQueue.value.orEmpty().isEmpty()) {
                    _playbackState.value = PlaybackState.IDLE
                    return
                }
                if (isRoomOwner.value == true) {
                    enableReverb(false)
                }
                _playbackState.value = PlaybackState.STOP
                if (isAwaitingScoreDisplay && _isScoringEnabled.value == true) {
                    mainHandler.postDelayed({
                        _songQueue.value?.size?.let {
                            if (it <= 1) {
                                _playbackState.value = PlaybackState.IDLE
                            }
                            playNextSongInQueue()
                        }
                    }, 5000)
                } else {
                    playNextSongInQueue()
                }
            }

            override fun onMusicProgressUpdated(progressMs: Long, durationMs: Long) {
                _playbackProgressMs.value = progressMs
                if (durationMs != songDurationMs.value) {
                    _songDurationMs.value = durationMs
                }
                if (isRoomOwner.value == false) {
                    if (isAwaitingScoreDisplay && progressMs / 1000 != durationMs / 1000) {
                        isAwaitingScoreDisplay = false
                    } else if (!isAwaitingScoreDisplay && progressMs / 1000 == durationMs / 1000) {
                        isAwaitingScoreDisplay = true
                    }
                }
            }

            override fun onVoicePitchUpdated(pitch: Int, hasVoice: Boolean, progressMs: Long) {}

            override fun onVoiceScoreUpdated(
                currentScore: Int,
                averageScore: Int,
                currentLine: Int,
            ) {
            }

            override fun shouldDecryptAudioData(audioData: ByteBuffer) {}
        }

    private fun parsePlayQueue(map: Map<String?, String?>): List<MusicSelection> {
        val json = map[KEY_PLAY_QUEUE]
        if (json.isNullOrEmpty()) {
            return emptyList()
        }
        return try {
            val type = object : TypeToken<List<MusicSelection>>() {}.type
            gson.fromJson(json, type)
        } catch (e: Exception) {
            e.printStackTrace()
            emptyList()
        }
    }

    private fun parseEnableScore(map: HashMap<String?, String?>): Boolean {
        return map[KEY_ENABLE_SCORE]?.toBoolean() ?: false
    }

    private fun applyDefaultAudioEffects() {
        enableDsp()
        enableHIFI()
        enableAIECModel2()
        enableAIEC()
        enableAI()
    }

    private fun callTRTCExperimentalApi(api: String, params: Map<String, Any>) {
        val json = gson.toJson(mapOf("api" to api, "params" to params))
        trtcCloud.callExperimentalAPI(json)
    }

    private fun enableDsp() {
        val params = mapOf(
            "configs" to listOf(
                mapOf(
                    "key" to "Liteav.Audio.common.dsp.version",
                    "value" to "2",
                    "default" to "1"
                )
            )
        )
        callTRTCExperimentalApi("setPrivateConfig", params)
    }

    private fun enableHIFI() {
        val params = mapOf(
            "configs" to listOf(
                mapOf(
                    "key" to "Liteav.Audio.common.smart.3a.strategy.flag",
                    "value" to "16",
                    "default" to "1"
                )
            )
        )
        callTRTCExperimentalApi("setPrivateConfig", params)
    }

    private fun enableAIECModel2() {
        val params = mapOf(
            "configs" to listOf(
                mapOf(
                    "key" to "Liteav.Audio.common.ai.ec.model.type",
                    "value" to "2",
                    "default" to "2"
                )
            )
        )
        callTRTCExperimentalApi("setPrivateConfig", params)
    }

    private fun enableAIEC() {
        val params = mapOf(
            "configs" to listOf(
                mapOf(
                    "key" to "Liteav.Audio.common.enable.ai.ec.module",
                    "value" to "1",
                    "default" to "1"
                )
            )
        )
        callTRTCExperimentalApi("setPrivateConfig", params)
    }

    private fun enableAI() {
        val params = mapOf(
            "configs" to listOf(
                mapOf(
                    "key" to "Liteav.Audio.common.ai.module.enabled",
                    "value" to "1",
                    "default" to "1"
                )
            )
        )
        callTRTCExperimentalApi("setPrivateConfig", params)
    }

    private fun enableReverb(enable: Boolean) {
        val params = mapOf(
            "enable" to enable,
            "RoomSize" to 60,
            "PreDelay" to 20,
            "Reverberance" to 40,
            "Damping" to 50,
            "ToneLow" to 30,
            "ToneHigh" to 100,
            "WetGain" to -3,
            "DryGain" to 0,
            "StereoWidth" to 40,
            "WetOnly" to false
        )
        callTRTCExperimentalApi("setCustomReverbParams", params)
    }


    private fun copyAllAssetsToStorage() {
        val assetFiles = listOf(
            "localMusicName.mp3", "localMusicName.vtt"
        )
        assetFiles.forEach { copyAssetToFile(it) }
    }

    private fun copyAssetToFile(assetName: String) {
        val savePath = ContextCompat.getExternalFilesDirs(context, null)[0].absolutePath
        val destinationFile = File(savePath, assetName)
        if (destinationFile.exists()) return
        try {
            destinationFile.parentFile?.mkdirs()
            context.assets.open(assetName).use { inputStream ->
                FileOutputStream(destinationFile).use { outputStream ->
                    inputStream.copyTo(outputStream)
                }
            }
        } catch (e: Exception) {
            e.printStackTrace()
        }
    }
}