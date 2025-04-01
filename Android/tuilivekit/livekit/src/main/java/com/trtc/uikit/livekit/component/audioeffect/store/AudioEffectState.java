package com.trtc.uikit.livekit.component.audioeffect.store;

import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceChangerType;
import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_0;
import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceReverbType;
import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_0;

import androidx.lifecycle.MutableLiveData;

public class AudioEffectState {
    public String                              roomId                = "";
    public MutableLiveData<Integer>            voiceVolume           = new MutableLiveData<>(100);
    public MutableLiveData<Integer>            musicVolume           = new MutableLiveData<>(60);
    public MutableLiveData<Integer>            earMonitorVolume      = new MutableLiveData<>(100);
    public MutableLiveData<Boolean>            enableVoiceEarMonitor = new MutableLiveData<>(false);
    public MutableLiveData<TXVoiceChangerType> changerType           = new MutableLiveData<>(TXLiveVoiceChangerType_0);
    public MutableLiveData<TXVoiceReverbType>  reverbType            = new MutableLiveData<>(TXLiveVoiceReverbType_0);

}
