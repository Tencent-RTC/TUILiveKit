package com.trtc.uikit.component.audioeffect.service;

import android.util.Log;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.component.audioeffect.store.AudioEffectState;
import com.trtc.uikit.component.common.StateCache;

import java.util.HashMap;
import java.util.Map;

public class AudioEffectService {

    private static final String TAG       = "AudioEffectService";
    private static final String STATE_KEY = "key_state_audio_effect";

    private final String           mRoomId;
    private final TRTCCloud        mTRTCCloud;
    public        AudioEffectState mAudioEffectState;

    public AudioEffectService(String roomId) {
        mRoomId = roomId;
        mTRTCCloud = TUIRoomEngine.sharedInstance().getTRTCCloud();
        mAudioEffectState = getState();
    }

    private AudioEffectState getState() {
        StateCache stateCache = StateCache.getInstance();
        Map<String, Object> map = stateCache.get(mRoomId);
        if (map == null) {
            map = new HashMap<>();
            stateCache.set(mRoomId, map);
        }
        AudioEffectState state = (AudioEffectState) map.get(STATE_KEY);
        if (state == null) {
            state = new AudioEffectState();
            map.put(STATE_KEY, state);
        }
        return state;
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
