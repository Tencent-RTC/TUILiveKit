package com.trtc.uikit.livekit.common.core.controller;

import static com.trtc.uikit.livekit.liveroom.data.MusicInfo.INVALID_ID;

import com.tencent.cloud.tuikit.engine.common.TUICommonDefine;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.qcloud.tuicore.TUIConfig;
import com.tencent.qcloud.tuicore.permission.PermissionCallback;
import com.trtc.tuikit.common.livedata.Observer;
import com.trtc.uikit.livekit.common.core.error.ErrorHandler;
import com.trtc.uikit.livekit.common.core.service.RoomEngineService;
import com.trtc.uikit.livekit.common.core.store.state.LiveState;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.common.utils.PermissionRequest;
import com.trtc.uikit.livekit.liveroom.data.MusicInfo;

public class MediaController extends Controller {
    private static final String TAG = "MediaController";

    private final Observer<Boolean> isInSeatObserver = this::onSelfInSeatStateChanged;

    public MediaController(LiveState state, RoomEngineService service) {
        super(state, service);
        mUserState.selfInfo.isInSeat.observe(isInSeatObserver);
    }

    @Override
    public void destroy() {
        mUserState.selfInfo.isInSeat.removeObserver(isInSeatObserver);
    }

    public void operateMicrophone() {
        if (!mMediaState.isMicrophoneOpened.get()) {
            openLocalMicrophone();
            return;
        }
        boolean isMuted = mMediaState.isMicrophoneMuted.get();
        LiveKitLog.info(TAG + " operateMicrophone isMuted:" + isMuted);
        if (isMuted) {
            unMuteLocalAudio();
        } else {
            muteLocalAudio();
        }
    }

    public void closeLocalCamera() {
        mRoomEngineService.closeLocalCamera();
    }

    private void openLocalMicrophone() {
        if (mMediaState.hasMicrophonePermission.get()) {
            openLocalMicrophoneByService();
            return;
        }
        PermissionRequest.requestMicrophonePermissions(TUIConfig.getAppContext(), new PermissionCallback() {
            @Override
            public void onRequesting() {
                LiveKitLog.info(TAG + " requestMicrophonePermissions onRequesting");
            }

            @Override
            public void onGranted() {
                LiveKitLog.info(TAG + " requestMicrophonePermissions onGranted");
                mMediaState.hasMicrophonePermission.set(true);
                openLocalMicrophoneByService();
            }

            @Override
            public void onDenied() {
                LiveKitLog.warn(TAG + " requestMicrophonePermissions onDenied");
            }
        });

    }

    private void openLocalMicrophoneByService() {
        unMuteLocalAudio();
        mRoomEngineService.openLocalMicrophone(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " operateMicrophone success");
                mMediaState.isMicrophoneOpened.set(true);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " openLocalMicrophone:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    private void unMuteLocalAudio() {
        mRoomEngineService.unMuteLocalAudio(new TUIRoomDefine.ActionCallback() {
            @Override
            public void onSuccess() {
                LiveKitLog.info(TAG + " unMuteLocalAudio success");
                mMediaState.isMicrophoneMuted.set(false);
            }

            @Override
            public void onError(TUICommonDefine.Error error, String message) {
                LiveKitLog.error(TAG + " unMuteLocalAudio:[Error] error:" + error + ",message:" + message + "]");
                ErrorHandler.onError(error);
            }
        });
    }

    private void muteLocalAudio() {
        mRoomEngineService.muteLocalAudio();
        mMediaState.isMicrophoneMuted.set(true);
    }

    public void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type) {
        mRoomEngineService.setVoiceChangerType(type);
        mMediaState.audioInfo.changerType.set(type);
    }

    public void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type) {
        mRoomEngineService.setVoiceReverbType(type);
        mMediaState.audioInfo.reverbType.set(type);
    }

    public void enableVoiceEarMonitor(boolean enable) {
        mRoomEngineService.enableVoiceEarMonitor(enable);
        mMediaState.audioInfo.enableVoiceEarMonitor.set(enable);
    }

    public void operatePlayMusic(MusicInfo musicInfo) {
        MusicInfo currentMusicInfo = mMediaState.currentMusicInfo.get();
        if (currentMusicInfo.id != INVALID_ID && currentMusicInfo.id != musicInfo.id) {
            stopMusic(currentMusicInfo.id);
        }
        mMediaState.currentMusicInfo.set(musicInfo);
        if (musicInfo.isPlaying.get()) {
            stopMusic(musicInfo.id);
        } else {
            startMusic(musicInfo);
        }
    }

    private void startMusic(MusicInfo musicInfo) {
        int musicVolume = mMediaState.audioInfo.musicVolume.get();
        mRoomEngineService.startMusic(musicInfo.id, musicInfo.path, musicInfo.pitch.get(), musicVolume);
        mMediaState.currentMusicInfo.get().isPlaying.set(true);
    }

    private void stopMusic(int id) {
        mRoomEngineService.stopMusic(id);
        mMediaState.currentMusicInfo.get().isPlaying.set(false);
    }

    public void deleteMusic(MusicInfo musicInfo) {
        if (musicInfo.isPlaying.get()) {
            stopMusic(musicInfo.id);
        }
        mMediaState.currentMusicInfo.set(new MusicInfo());
        mMediaState.musicList.remove(musicInfo);
    }

    public void setMusicVolume(int volume) {
        mRoomEngineService.setMusicVolume(volume);
        mMediaState.audioInfo.musicVolume.set(volume);
    }

    public void setVoiceEarMonitorVolume(int volume) {
        mRoomEngineService.setVoiceEarMonitorVolume(volume);
        mMediaState.audioInfo.earMonitorVolume.set(volume);
    }

    public void setVoiceVolume(int volume) {
        mRoomEngineService.setVoiceVolume(volume);
        mMediaState.audioInfo.voiceVolume.set(volume);
    }

    private void onSelfInSeatStateChanged(boolean isInSeat) {
        LiveKitLog.info(TAG + " onSelfInSeatStateChanged isInSeat:" + isInSeat);
        if (isInSeat) {
            openLocalMicrophone();
        }
    }
}
