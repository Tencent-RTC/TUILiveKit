package com.trtc.uikit.livekit.common.core.store.state.operation;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.liveroom.data.AudioInfo;
import com.trtc.uikit.livekit.liveroom.data.MusicInfo;

import java.util.ArrayList;
import java.util.List;

public class MediaState {
    public LiveData<Boolean>   hasMicrophonePermission = new LiveData<>(false);
    public LiveData<Boolean>   isMicrophoneOpened      = new LiveData<>(false);
    public LiveData<Boolean>   isMicrophoneMuted       = new LiveData<>(true);
    public LiveData<MusicInfo> currentMusicInfo        = new LiveData<>(new MusicInfo());
    public List<MusicInfo>     musicList               = new ArrayList<>();
    public AudioInfo           audioInfo               = new AudioInfo();
}
