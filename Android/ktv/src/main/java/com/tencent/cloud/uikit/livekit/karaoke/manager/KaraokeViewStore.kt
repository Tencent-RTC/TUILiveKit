package com.tencent.cloud.uikit.livekit.karaoke.manager

import android.content.Context
import com.google.gson.Gson
import com.tencent.cloud.tuikit.engine.common.TUICommonDefine
import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine
import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine
import com.tencent.cloud.tuikit.engine.room.TUIRoomObserver
import com.tencent.cloud.uikit.ktv.R
import com.tencent.cloud.uikit.livekit.karaoke.state.LiveStatus
import com.tencent.cloud.uikit.livekit.karaoke.state.MusicPitchModel
import com.tencent.cloud.uikit.livekit.karaoke.state.PlayState
import com.tencent.cloud.uikit.livekit.karaoke.view.lyric.LyricInfo
import com.tencent.cloud.uikit.livekit.utils.Logger
import com.tencent.cloud.uikit.livekit.utils.PermissionRequest
import com.tencent.cloud.uikit.livekit.utils.ToastUtils
import com.tencent.liteav.audio.TXAudioEffectManager
import com.tencent.trtc.TRTCCloudDef
import com.tencent.trtc.TRTCCloudListener
import com.trtc.tuikit.common.permission.PermissionCallback
import com.trtc.tuikit.common.system.ContextProvider
import com.trtc.uikit.livekit.voiceroomcore.manager.VoiceRoomManager
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.MainScope
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch
import kotlinx.coroutines.suspendCancellableCoroutine
import org.json.JSONException
import org.json.JSONObject
import kotlin.coroutines.resume

private const val ORIGIN_ID = 0
private const val ACCOMPANY_ID = 1

data class KaraokeSEIData(
    var musicId: String,
    var currentTime: Long,
    var totalTime: Long
)

class KaraokeStore() {
    val logger: Logger = Logger.getKTVLogger("KaraokeStore")

    val trtcCloud = TUIRoomEngine.sharedInstance().trtcCloud

    private var voiceRoomManager: VoiceRoomManager = VoiceRoomManager()

    val audioEffectManager = trtcCloud.audioEffectManager

    val _currentPlayProgress = MutableStateFlow(0L)

    val _currentPlayDuration = MutableStateFlow(0L)

    val _isAccompany = MutableStateFlow(false)

    val _playState = MutableStateFlow(PlayState.STOPPED)

    val _standardPitchModels = MutableStateFlow<MutableList<MusicPitchModel>>(mutableListOf())

    val _currentPitch = MutableStateFlow(0)

    val _playScore = MutableStateFlow(0)

    val _isMicrophoneMuted = MutableStateFlow(true)

    val _liveStatus = MutableStateFlow(LiveStatus.IDLE)

    var currentPlayMusicId = ""

    var isMicrophoneOpened = false

    var liveId = ""

    private val mainScope: CoroutineScope = MainScope()

    private val roomEngineObserver = object : TUIRoomObserver() {
        override fun onRoomDismissed(roomId: String?, reason: TUIRoomDefine.RoomDismissedReason?) {
            _liveStatus.update {
                LiveStatus.ENDED
            }
        }
    }

    private val trtcCloudListener = object : TRTCCloudListener() {
        override fun onRecvSEIMsg(userId: String?, data: ByteArray?) {
            val gson = Gson()
            val result = data?.let { String(it) }
            val jsonData = gson.fromJson(result, KaraokeSEIData::class.java)
            _currentPlayProgress.update {
                jsonData.currentTime
            }
            _currentPlayDuration.update {
                jsonData.totalTime
            }
        }

        override fun onUserVideoAvailable(userId: String?, available: Boolean) {
            if (available) {
                trtcCloud.startRemoteView(userId, TRTCCloudDef.TRTC_VIDEO_STREAM_TYPE_SMALL, null)
            } else {
                trtcCloud.stopRemoteView(userId, TRTCCloudDef.TRTC_VIDEO_STREAM_TYPE_SMALL)
            }
        }
    }

    private val musicPlayListener = object : TXAudioEffectManager.TXMusicPlayObserver {
        override fun onStart(id: Int, errCode: Int) {
            logger.info("onMusicPlay Start: id = $id , errCode = $errCode")
            if (id == ORIGIN_ID) {
                if (errCode == 0) {
                    _playState.update {
                        PlayState.PLAYING
                    }
                }
            }
        }

        override fun onPlayProgress(id: Int, curPtsMS: Long, durationMS: Long) {
            if (id == ORIGIN_ID) {
                sendSEIMsg(currentPlayMusicId, curPtsMS, durationMS)
                _currentPlayProgress.update {
                    curPtsMS
                }
                _currentPlayDuration.update {
                    durationMS
                }
                _currentPitch.update {
                    (20..80).random()
                }
            }
        }

        override fun onComplete(id: Int, errCode: Int) {
            logger.info("onMusicPlayComplete id $id , status = $errCode")
            if (id == ORIGIN_ID) {
                _playState.update {
                    PlayState.STOPPED
                }
                _playScore.update {
                    (80..100).random()
                }
            }
            enableBlackStream(false)
        }
    }

    init {
        trtcCloud.addListener(trtcCloudListener)
        TUIRoomEngine.sharedInstance().addObserver(roomEngineObserver)
    }

    fun cleanData() {
        trtcCloud.removeListener(trtcCloudListener)
        stopPlayMusic()
        closeLocalMicrophone()
        voiceRoomManager.destroy()
        TUIRoomEngine.sharedInstance().removeObserver(roomEngineObserver)
    }

    fun generateStandardPitchModels(lyricInfo: LyricInfo?) {
        if (lyricInfo == null) {
            return
        }
        val list = ArrayList<MusicPitchModel>(lyricInfo.lineList.size)
        lyricInfo.lineList.forEach { lineInfo ->
            val lineStartTime = lineInfo.start
            val linePitch = (20..80).random()
            lineInfo.wordList.forEach {
                val startTime = it.offset + lineStartTime
                val pitch = linePitch + (-5..5).random()
                val patchModel = MusicPitchModel(startTime, it.duration, pitch)
                list.add(patchModel)
            }
        }
        _standardPitchModels.update { list }
    }

    fun startPlayMusic(
        musicID: String,
        originalUrl: String,
        accompanyUrl: String
    ) {
        logger.info("startPlayMusic id:$musicID,originalUrl:$originalUrl,accompanyUrl$accompanyUrl")
        currentPlayMusicId = musicID
        enableBlackStream(true)
        switchAccompany(_isAccompany.value)
        val originParams = TXAudioEffectManager.AudioMusicParam(ORIGIN_ID, originalUrl).apply {
            publish = true
        }
        audioEffectManager.startPlayMusic(originParams)
        audioEffectManager.setMusicObserver(ORIGIN_ID, musicPlayListener)

        val accompanyParam = TXAudioEffectManager.AudioMusicParam(ACCOMPANY_ID, accompanyUrl).apply {
            publish = true
        }
        audioEffectManager.startPlayMusic(accompanyParam)
        audioEffectManager.setMusicObserver(ACCOMPANY_ID, musicPlayListener)
    }

    fun stopPlayMusic() {
        logger.info("stopPlayMusic")
        audioEffectManager.stopPlayMusic(ORIGIN_ID)
        audioEffectManager.stopPlayMusic(ACCOMPANY_ID)
        audioEffectManager.setMusicObserver(ORIGIN_ID, null)
        audioEffectManager.setMusicObserver(ACCOMPANY_ID, null)
        enableBlackStream(false)
        _playState.update {
            PlayState.STOPPED
        }
        _currentPlayProgress.update {
            0
        }
        _currentPlayDuration.update {
            0
        }
        _currentPitch.update {
            0
        }
    }

    fun switchAccompany(isAccompany: Boolean) {
        logger.info("switchAccompany isAccompany:$isAccompany")
        val localMusicVolume = 60
        val localAccompanyVolume = 60
        val remoteMusicVolume = 80
        val remoteAccompanyVolume = 95
        if (isAccompany) {
            audioEffectManager.setMusicPlayoutVolume(ORIGIN_ID, 0)
            audioEffectManager.setMusicPublishVolume(ORIGIN_ID, 0)
            audioEffectManager.setMusicPlayoutVolume(ACCOMPANY_ID, localAccompanyVolume)
            audioEffectManager.setMusicPublishVolume(ACCOMPANY_ID, remoteAccompanyVolume)
        } else {
            audioEffectManager.setMusicPlayoutVolume(ORIGIN_ID, localMusicVolume)
            audioEffectManager.setMusicPublishVolume(ORIGIN_ID, remoteMusicVolume)
            audioEffectManager.setMusicPlayoutVolume(ACCOMPANY_ID, 0)
            audioEffectManager.setMusicPublishVolume(ACCOMPANY_ID, 0)
        }
        _isAccompany.update {
            isAccompany
        }
    }

    fun sendSEIMsg(musicId: String, curTime: Long, duration: Long) {
        val jsonData = KaraokeSEIData(musicId, curTime, duration)
        val gson = Gson()
        val data = gson.toJson(jsonData)
        trtcCloud.sendSEIMsg(data.toByteArray(), 1)
    }

    fun enableBlackStream(enable: Boolean) {
        val jsonObject = JSONObject()
        try {
            jsonObject.put("api", "enableBlackStream")
            val params = JSONObject()
            params.put("enable", enable)
            jsonObject.put("params", params)
            trtcCloud.callExperimentalAPI(jsonObject.toString())
        } catch (e: JSONException) {
            e.printStackTrace()
        }
    }

    private suspend fun requestMicrophonePermissionSuspend(context: Context): Boolean =
        suspendCancellableCoroutine { cont ->
            PermissionRequest.requestMicrophonePermissions(context, object : PermissionCallback() {
                override fun onGranted() {
                    cont.resume(true)
                }

                override fun onDenied() {
                    cont.resume(false)
                }
            })
        }

    fun openLocalMicrophone() {
        logger.info("openLocalMicrophone")
        mainScope.launch {
            val context = ContextProvider.getApplicationContext()
            if (requestMicrophonePermissionSuspend(context)) {
                logger.info("requestMicrophonePermissions onGranted")
                setAudioEffect(trtcCloud)
                trtcCloud.startLocalAudio(TRTCCloudDef.TRTC_AUDIO_QUALITY_MUSIC)
                trtcCloud.audioCaptureVolume = 100
                _isMicrophoneMuted.update { false }
                isMicrophoneOpened = true
            } else {
                logger.info("requestMicrophonePermissions onDenied")
                ToastUtils.showToast(context, context.getString(R.string.ktv_microphone_permission_tips))
            }
        }
    }

    fun closeLocalMicrophone() {
        logger.info("closeLocalMicrophone")
        trtcCloud.stopLocalAudio()
        trtcCloud.audioCaptureVolume = 100
        _isMicrophoneMuted.update {
            true
        }
        isMicrophoneOpened = false
    }

    fun muteLocalAudio() {
        logger.info("muteLocalAudio")
        trtcCloud.audioCaptureVolume = 0
        _isMicrophoneMuted.update {
            true
        }
    }

    fun unMuteLocalAudio() {
        logger.info("unMuteLocalAudio")
        if (!isMicrophoneOpened) {
            mainScope.launch {
                openLocalMicrophone()
            }
            return
        }
        trtcCloud.audioCaptureVolume = 100
        _isMicrophoneMuted.update {
            false
        }
    }

    fun getVoiceRoomManager(): VoiceRoomManager {
        return voiceRoomManager
    }

    fun createLive(liveInfo: TUILiveListManager.LiveInfo) {
        this.liveId = liveInfo.roomId
        voiceRoomManager.startVoiceRoom(liveInfo, object : TUILiveListManager.LiveInfoCallback {
            override fun onSuccess(liveInfo: TUILiveListManager.LiveInfo?) {
                _liveStatus.update {
                    LiveStatus.LIVE
                }
                logger.info("createLive onSuccess")
            }

            override fun onError(error: TUICommonDefine.Error?, message: String?) {
                logger.error("createLive onError error:$error message:$message")
            }
        })
    }

    fun endLive() {
        voiceRoomManager.stopVoiceRoom(object : TUILiveListManager.StopLiveCallback {
            override fun onSuccess(statisticData: TUILiveListManager.LiveStatisticsData?) {
                _liveStatus.update {
                    LiveStatus.ENDED
                }
                logger.info("endLive onSuccess")
            }

            override fun onError(error: TUICommonDefine.Error?, message: String?) {
                logger.error("endLive onError error:$error message:$message")
            }
        })
    }

    fun joinLive(liveId: String) {
        this.liveId = liveId
        voiceRoomManager.joinVoiceRoom(liveId, object : TUILiveListManager.LiveInfoCallback {
            override fun onSuccess(liveInfo: TUILiveListManager.LiveInfo?) {
                _liveStatus.update {
                    LiveStatus.LIVE
                }
                logger.info("joinLive onSuccess")
            }

            override fun onError(error: TUICommonDefine.Error?, message: String?) {
                logger.error("joinLive onError error:$error message:$message")
            }
        })
    }

    fun leaveLive() {
        voiceRoomManager.leaveVoiceRoom(object : TUIRoomDefine.ActionCallback {
            override fun onSuccess() {
                logger.info("leaveLive onSuccess")
            }

            override fun onError(error: TUICommonDefine.Error?, message: String?) {
                logger.error("leaveLive onError error:$error message:$message")
            }
        })
    }
}