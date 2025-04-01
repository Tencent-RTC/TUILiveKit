package com.trtc.uikit.livekit.component.audioeffect.service;

import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_0;
import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_0;

import android.util.Log;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.livekit.component.audioeffect.store.AudioEffectState;
import com.trtc.uikit.livekit.component.audioeffect.store.AudioEffectStore;

public class AudioEffectService {

    private static final String    TAG = "AudioEffectService";
    private final        TRTCCloud mTRTCCloud;

    public AudioEffectService() {
        mTRTCCloud = TUIRoomEngine.sharedInstance().getTRTCCloud();
    }

    public void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type) {
        Log.i(TAG, "setVoiceChangerType:[type:" + type + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceChangerType(type);
        AudioEffectStore.sharedInstance().mAudioEffectState.changerType.setValue(type);
    }

    public void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type) {
        Log.i(TAG, "setVoiceReverbType:[type:" + type + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceReverbType(type);
        AudioEffectStore.sharedInstance().mAudioEffectState.reverbType.setValue(type);
    }

    public void enableVoiceEarMonitor(boolean enable) {
        Log.i(TAG, "enableVoiceEarMonitor:[enable:" + enable + "]");
        mTRTCCloud.getAudioEffectManager().enableVoiceEarMonitor(enable);
        AudioEffectStore.sharedInstance().mAudioEffectState.enableVoiceEarMonitor.setValue(enable);
    }

    public void setVoiceEarMonitorVolume(int volume) {
        Log.i(TAG, "setVoiceEarMonitorVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceEarMonitorVolume(volume);
        AudioEffectStore.sharedInstance().mAudioEffectState.earMonitorVolume.setValue(volume);
    }

    public void setVoiceVolume(int volume) {
        Log.i(TAG, "setVoiceVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceCaptureVolume(volume);
        AudioEffectStore.sharedInstance().mAudioEffectState.voiceVolume.setValue(volume);
    }

    public void resetSettings() {
        Log.i(TAG, "resetSettings:[]");
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
