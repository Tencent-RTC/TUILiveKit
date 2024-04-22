package com.trtc.uikit.livekit.liveroom.data;

import com.trtc.tuikit.common.livedata.LiveData;

public class MusicInfo {
    public static final int INVALID_ID = -1;

    public int               id        = INVALID_ID;
    public String            name;
    public String            path;
    public LiveData<Boolean> isPlaying = new LiveData<>(false);
    public LiveData<Float>   pitch     = new LiveData<>(0.0f);

    public MusicInfo() {
    }

    public MusicInfo(int id, String name, String musicPath) {
        this.id = id;
        this.name = name;
        this.path = musicPath;
    }
}
