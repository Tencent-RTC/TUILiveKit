package com.trtc.uikit.livekit.common.uicomponent.music.service;

import static com.trtc.uikit.livekit.common.uicomponent.music.store.MusicStore.MusicInfo.INVALID_ID;

import com.trtc.uikit.livekit.common.uicomponent.music.store.MusicStore;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;
import com.trtc.uikit.livekit.manager.LiveController;

public class MusicService {

    public static final String TAG = "MusicService";

    public LiveController mLiveController;
    public MusicStore     mMusicStore;

    public MusicService(LiveController liveController) {
        mLiveController = liveController;
        mMusicStore = MusicStore.getInstance();
    }

    public void operatePlayMusic(MusicStore.MusicInfo musicInfo) {
        MusicStore.MusicInfo currentMusicInfo = mMusicStore.currentMusicInfo.get();
        if (currentMusicInfo.id != INVALID_ID && currentMusicInfo.id != musicInfo.id) {
            stopMusic(currentMusicInfo.id);
        }
        mMusicStore.currentMusicInfo.set(musicInfo);
        boolean isPlaying = musicInfo.isPlaying.get();
        LiveKitLog.info(TAG + "operatePlayMusic:[isPlaying:" + isPlaying + "]");
        if (musicInfo.isPlaying.get()) {
            stopMusic(musicInfo.id);
        } else {
            startMusic(musicInfo);
        }
    }

    private void startMusic(MusicStore.MusicInfo musicInfo) {
        LiveKitLog.info(TAG + "[" + mLiveController.getRoomSate().roomId + "] startMusic:[musicInfo:" + musicInfo
                + "]");
        mLiveController.getLiveService().startMusic(musicInfo.id, musicInfo.path, musicInfo.pitch.get());
        mMusicStore.currentMusicInfo.get().isPlaying.set(true);
    }

    private void stopMusic(int id) {
        LiveKitLog.info(TAG + "[" + mLiveController.getRoomSate().roomId + "] stopMusic:[id:" + id + "]");
        mLiveController.getLiveService().stopMusic(id);
        mMusicStore.currentMusicInfo.get().isPlaying.set(false);
    }

    public void deleteMusic(MusicStore.MusicInfo musicInfo) {
        boolean isPlaying = musicInfo.isPlaying.get();
        LiveKitLog.info(TAG + "[" + mLiveController.getRoomSate().roomId + "] deleteMusic:[musicInfo:" + musicInfo
                + ",isPlaying:" + isPlaying + "]");
        if (isPlaying) {
            stopMusic(musicInfo.id);
        }
        mMusicStore.currentMusicInfo.set(new MusicStore.MusicInfo());
        mMusicStore.musicList.remove(musicInfo);
    }
}
