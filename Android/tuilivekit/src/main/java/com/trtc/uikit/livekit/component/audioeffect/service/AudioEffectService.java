package com.trtc.uikit.livekit.component.audioeffect.service;

import android.util.Log;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.livekit.component.audioeffect.store.AudioEffectSateFactory;
import com.trtc.uikit.livekit.component.audioeffect.store.AudioEffectState;

public class AudioEffectService {

    public static final String TAG = "AudioEffectService";

    private final String           mRoomId;
    private final TRTCCloud        mTRTCCloud;
    public        AudioEffectState mAudioEffectState;

    public AudioEffectService(String roomId) {
        mRoomId = roomId;
        mTRTCCloud = TUIRoomEngine.sharedInstance().getTRTCCloud();
        mAudioEffectState = AudioEffectSateFactory.getState(roomId);
    }

    public void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type) {
        Log.i(TAG, "[" + mRoomId + "] setVoiceChangerType:[type:" + type
                + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceChangerType(type);
        mAudioEffectState.changerType.set(type);
    }

    public void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type) {
        Log.i(TAG, "[" + mRoomId + "] setVoiceReverbType:[type:" + type + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceReverbType(type);
        mAudioEffectState.reverbType.set(type);
    }

    public void enableVoiceEarMonitor(boolean enable) {
        Log.i(TAG, "[" + mRoomId + "] enableVoiceEarMonitor:[enable:" + enable + "]");
        mTRTCCloud.getAudioEffectManager().enableVoiceEarMonitor(enable);
        mAudioEffectState.enableVoiceEarMonitor.set(enable);
    }

    public void setMusicVolume(int volume) {
        Log.i(TAG, "[" + mRoomId + "] setMusicVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setAllMusicVolume(volume);
        mAudioEffectState.musicVolume.set(volume);
    }

    public void setVoiceEarMonitorVolume(int volume) {
        Log.i(TAG, "[" + mRoomId + "] setVoiceEarMonitorVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceEarMonitorVolume(volume);
        mAudioEffectState.earMonitorVolume.set(volume);
    }

    public void setVoiceVolume(int volume) {
        Log.i(TAG, "[" + mRoomId + "] setVoiceVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceCaptureVolume(volume);
        mAudioEffectState.voiceVolume.set(volume);
    }
}
