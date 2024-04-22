package com.trtc.uikit.livekit.liveroom.data;

import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceChangerType.TXLiveVoiceChangerType_0;
import static com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceReverbType.TXLiveVoiceReverbType_0;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.AudioQuality;
import com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceChangerType;
import com.tencent.liteav.audio.TXAudioEffectManager.TXVoiceReverbType;
import com.trtc.tuikit.common.livedata.LiveData;

public class AudioInfo {
    public LiveData<Integer>            voiceVolume           = new LiveData<>(100);
    public LiveData<Integer>            musicVolume           = new LiveData<>(100);
    public LiveData<Integer>            earMonitorVolume      = new LiveData<>(100);
    public LiveData<Boolean>            muteAudio             = new LiveData<>(false);
    public LiveData<Boolean>            enableVoiceEarMonitor = new LiveData<>(false);
    public LiveData<TXVoiceChangerType> changerType           = new LiveData<>(TXLiveVoiceChangerType_0);
    public LiveData<TXVoiceReverbType>  reverbType            = new LiveData<>(TXLiveVoiceReverbType_0);
    public LiveData<AudioQuality>       audioQuality          = new LiveData<>(AudioQuality.DEFAULT);
}
