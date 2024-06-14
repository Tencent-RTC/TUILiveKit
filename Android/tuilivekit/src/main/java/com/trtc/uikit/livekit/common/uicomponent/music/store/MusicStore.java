package com.trtc.uikit.livekit.common.uicomponent.music.store;

import com.trtc.tuikit.common.livedata.LiveData;

import java.util.ArrayList;
import java.util.List;

public class MusicStore {

    public LiveData<MusicInfo> currentMusicInfo = new LiveData<>(new MusicInfo());
    public List<MusicInfo>     musicList        = new ArrayList<>();

    private MusicStore() {
    }

    private static class MusicStoreHolder {
        private static final MusicStore instance = new MusicStore();
    }

    public static MusicStore getInstance() {
        return MusicStore.MusicStoreHolder.instance;
    }

    public static class MusicInfo {
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
}
