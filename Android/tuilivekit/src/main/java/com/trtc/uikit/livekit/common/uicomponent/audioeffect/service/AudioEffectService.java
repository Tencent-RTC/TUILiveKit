package com.trtc.uikit.livekit.common.uicomponent.audioeffect.service;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.store.AudioEffectSateFactory;
import com.trtc.uikit.livekit.common.uicomponent.audioeffect.store.AudioEffectState;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

public class AudioEffectService {

    public static final String TAG = "AudioEffectService";

    private final String           mRoomId;
    private final TRTCCloud        mTRTCCloud;
    public        AudioEffectState mAudioEffectState;

    public AudioEffectService(String roomId, TRTCCloud trtcCloud) {
        mRoomId = roomId;
        mTRTCCloud = trtcCloud;
        mAudioEffectState = AudioEffectSateFactory.getState(roomId);
    }

    public void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type) {
        LiveKitLog.info(TAG + "[" + mRoomId + "] setVoiceChangerType:[type:" + type
                + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceChangerType(type);
        mAudioEffectState.changerType.set(type);
    }

    public void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type) {
        LiveKitLog.info(TAG + "[" + mRoomId + "] setVoiceReverbType:[type:" + type + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceReverbType(type);
        mAudioEffectState.reverbType.set(type);
    }

    public void enableVoiceEarMonitor(boolean enable) {
        LiveKitLog.info(TAG + "[" + mRoomId + "] enableVoiceEarMonitor:[enable:" + enable + "]");
        mTRTCCloud.getAudioEffectManager().enableVoiceEarMonitor(enable);
        mAudioEffectState.enableVoiceEarMonitor.set(enable);
    }

    public void setMusicVolume(int volume) {
        LiveKitLog.info(TAG + "[" + mRoomId + "] setMusicVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setAllMusicVolume(volume);
        mAudioEffectState.musicVolume.set(volume);
    }

    public void setVoiceEarMonitorVolume(int volume) {
        LiveKitLog.info(TAG + "[" + mRoomId + "] setVoiceEarMonitorVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceEarMonitorVolume(volume);
        mAudioEffectState.earMonitorVolume.set(volume);
    }

    public void setVoiceVolume(int volume) {
        LiveKitLog.info(TAG + "[" + mRoomId + "] setVoiceVolume:[volume:" + volume + "]");
        mTRTCCloud.getAudioEffectManager().setVoiceCaptureVolume(volume);
        mAudioEffectState.voiceVolume.set(volume);
    }
}
