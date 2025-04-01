package com.trtc.uikit.livekit.component.audioeffect.store;

import com.trtc.uikit.livekit.component.audioeffect.service.AudioEffectService;

public class AudioEffectStore {
    private static AudioEffectStore   sInstance;
    public final   AudioEffectState   mAudioEffectState;
    public final   AudioEffectService mAudioEffectService;

    public static AudioEffectStore sharedInstance() {
        if (sInstance == null) {
            synchronized (AudioEffectStore.class) {
                if (sInstance == null) {
                    sInstance = new AudioEffectStore();
                }
            }
        }
        return sInstance;
    }

    private AudioEffectStore() {
        mAudioEffectState = new AudioEffectState();
        mAudioEffectService = new AudioEffectService();
    }

    public void init(String roomId) {
        mAudioEffectState.roomId = roomId;
    }

    public void unInit() {
        mAudioEffectService.resetSettings();
    }
}
