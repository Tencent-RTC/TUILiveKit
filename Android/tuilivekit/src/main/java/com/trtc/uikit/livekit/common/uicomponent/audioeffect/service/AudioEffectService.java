package com.trtc.uikit.livekit.common.uicomponent.audioeffect.service;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.store.AudioEffectStore;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.LiveController;

public class AudioEffectService {

    public static final String TAG = "AudioEffectService";
    
    public LiveController   mLiveController;
    public AudioEffectStore mAudioEffectStore;

    public AudioEffectService(LiveController liveController) {
        mAudioEffectStore = AudioEffectStore.getInstance();
        mLiveController = liveController;
    }

    public void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type) {
        LiveKitLog.info(TAG + "[" + mLiveController.getRoomSate().roomId + "] setVoiceChangerType:[type:" + type
                + "]");
        mLiveController.getLiveService().setVoiceChangerType(type);
        mAudioEffectStore.changerType.set(type);
    }

    public void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type) {
        LiveKitLog.info(TAG + "[" + mLiveController.getRoomSate().roomId + "] setVoiceReverbType:[type:" + type + "]");
        mLiveController.getLiveService().setVoiceReverbType(type);
        mAudioEffectStore.reverbType.set(type);
    }

    public void enableVoiceEarMonitor(boolean enable) {
        LiveKitLog.info(TAG + "[" + mLiveController.getRoomSate().roomId + "] enableVoiceEarMonitor:[enable:" + enable
                + "]");
        mLiveController.getLiveService().enableVoiceEarMonitor(enable);
        mAudioEffectStore.enableVoiceEarMonitor.set(enable);
    }

    public void setMusicVolume(int volume) {
        LiveKitLog.info(TAG + "[" + mLiveController.getRoomSate().roomId + "] setMusicVolume:[volume:" + volume + "]");
        mLiveController.getLiveService().setMusicVolume(volume);
        mAudioEffectStore.musicVolume.set(volume);
    }

    public void setVoiceEarMonitorVolume(int volume) {
        LiveKitLog.info(TAG + "[" + mLiveController.getRoomSate().roomId + "] setVoiceEarMonitorVolume:[volume:"
                + volume + "]");
        mLiveController.getLiveService().setVoiceEarMonitorVolume(volume);
        mAudioEffectStore.earMonitorVolume.set(volume);
    }

    public void setVoiceVolume(int volume) {
        LiveKitLog.info(TAG + "[" + mLiveController.getRoomSate().roomId + "] setVoiceVolume:[volume:" + volume + "]");
        mLiveController.getLiveService().setVoiceVolume(volume);
        mAudioEffectStore.voiceVolume.set(volume);
    }
}
