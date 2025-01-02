package com.trtc.uikit.component.audioeffect.store;

import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_0;
import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_0;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.trtc.tuikit.common.livedata.LiveData;

public class AudioEffectState {
    public LiveData<Integer>                                 voiceVolume           = new LiveData<>(100);
    public LiveData<Integer>                                 musicVolume           = new LiveData<>(60);
    public LiveData<Integer>                                 earMonitorVolume      = new LiveData<>(100);
    public LiveData<Boolean>                                 enableVoiceEarMonitor = new LiveData<>(false);
    public LiveData<TXAudioEffectManager.TXVoiceChangerType> changerType           =
            new LiveData<>(TXLiveVoiceChangerType_0);
    public LiveData<TXAudioEffectManager.TXVoiceReverbType>  reverbType            =
            new LiveData<>(TXLiveVoiceReverbType_0);

}
