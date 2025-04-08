package com.trtc.uikit.livekit.component.audioeffect.service;

import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_0;
import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_0;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.livekit.common.LiveKitLogger;
import com.trtc.uikit.livekit.component.audioeffect.store.AudioEffectState;
import com.trtc.uikit.livekit.component.audioeffect.store.AudioEffectStore;

public class AudioEffectService {
    private static final LiveKitLogger LOGGER = LiveKitLogger.getComponentLogger("AudioEffectService");
    private final        TRTCCloud     mTRTCCloud;

    public AudioEffectService() {
        mTRTCCloud = TUIRoomEngine.sharedInstance().getTRTCCloud();
    }

    public void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type) {
        LOGGER.info("setVoiceChangerType:[type:" + type + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceChangerType(type);
        AudioEffectStore.sharedInstance().mAudioEffectState.changerType.setValue(type);
    }

    public void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type) {
        LOGGER.info("setVoiceReverbType:[type:" + type + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceReverbType(type);
        AudioEffectStore.sharedInstance().mAudioEffectState.reverbType.setValue(type);
    }

    public void enableVoiceEarMonitor(boolean enable) {
        LOGGER.info("enableVoiceEarMonitor:[enable:" + enable + "]");
        mTRTCCloud.getAudioEffectManager().enableVoiceEarMonitor(enable);
        AudioEffectStore.sharedInstance().mAudioEffectState.enableVoiceEarMonitor.setValue(enable);
    }

    public void setVoiceEarMonitorVolume(int volume) {
        LOGGER.info("setVoiceEarMonitorVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceEarMonitorVolume(volume);
        AudioEffectStore.sharedInstance().mAudioEffectState.earMonitorVolume.setValue(volume);
    }

    public void setVoiceVolume(int volume) {
        LOGGER.info("setVoiceVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceCaptureVolume(volume);
        AudioEffectStore.sharedInstance().mAudioEffectState.voiceVolume.setValue(volume);
    }

    public void resetSettings() {
        LOGGER.info("resetSettings:[]");
        AudioEffectState state = AudioEffectStore.sharedInstance().mAudioEffectState;
        state.roomId = "";
        state.voiceVolume.setValue(100);
        state.musicVolume.setValue(60);
        state.earMonitorVolume.setValue(100);
        state.enableVoiceEarMonitor.setValue(false);
        state.changerType.setValue(TXLiveVoiceChangerType_0);
        state.reverbType.setValue(TXLiveVoiceReverbType_0);

        TXAudioEffectManager audioEffectManager = mTRTCCloud.getAudioEffectManager();
        audioEffectManager.setVoiceChangerType(state.changerType.getValue());
        audioEffectManager.setVoiceReverbType(state.reverbType.getValue());
        audioEffectManager.enableVoiceEarMonitor(state.enableVoiceEarMonitor.getValue());
        audioEffectManager.setVoiceEarMonitorVolume(state.earMonitorVolume.getValue());
        audioEffectManager.setVoiceCaptureVolume(state.voiceVolume.getValue());
    }
}
