package com.trtc.uikit.livekit.common.uicomponent.audioeffect.store;

import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_0;
import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_0;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.trtc.tuikit.common.livedata.LiveData;

public class AudioEffectStore {

    private AudioEffectStore() {
    }

    private static class AudioEffectStoreHolder {
        private static final AudioEffectStore instance = new AudioEffectStore();
    }

    public static AudioEffectStore getInstance() {
        return AudioEffectStore.AudioEffectStoreHolder.instance;
    }

    public LiveData<Integer>                                 voiceVolume           = new LiveData<>(100);
    public LiveData<Integer>                                 musicVolume           = new LiveData<>(100);
    public LiveData<Integer>                                 earMonitorVolume      = new LiveData<>(100);
    public LiveData<Boolean>                                 enableVoiceEarMonitor = new LiveData<>(false);
    public LiveData<TXAudioEffectManager.TXVoiceChangerType> changerType           =
            new LiveData<>(TXLiveVoiceChangerType_0);
    public LiveData<TXAudioEffectManager.TXVoiceReverbType>  reverbType            =
            new LiveData<>(TXLiveVoiceReverbType_0);

}
