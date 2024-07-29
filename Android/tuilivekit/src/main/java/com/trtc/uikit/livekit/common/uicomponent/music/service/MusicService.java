package com.trtc.uikit.livekit.common.uicomponent.music.service;

import static com.trtc.uikit.livekit.common.uicomponent.music.store.MusicPanelState.MusicInfo.INVALID_ID;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.livekit.common.uicomponent.music.store.MusicPanelSateFactory;
import com.trtc.uikit.livekit.common.uicomponent.music.store.MusicPanelState;
import com.trtc.uikit.livekit.common.utils.LiveKitLog;

public class MusicService {
    public static final String TAG = "MusicService";

    private final String          mRoomId;
    private final TRTCCloud       mTRTCCloud;
    public        MusicPanelState mMusicPanelState;

    public MusicService(String roomId, TRTCCloud trtcCloud) {
        mRoomId = roomId;
        mTRTCCloud = trtcCloud;
        mMusicPanelState = MusicPanelSateFactory.getState(roomId);
    }

    public void operatePlayMusic(MusicPanelState.MusicInfo musicInfo) {
        MusicPanelState.MusicInfo currentMusicInfo = mMusicPanelState.currentMusicInfo.get();
        if (currentMusicInfo.id != INVALID_ID && currentMusicInfo.id != musicInfo.id) {
            stopMusic(currentMusicInfo.id);
        }
        mMusicPanelState.currentMusicInfo.set(musicInfo);
        boolean isPlaying = musicInfo.isPlaying.get();
        LiveKitLog.info(TAG + "operatePlayMusic:[isPlaying:" + isPlaying + "]");
        if (musicInfo.isPlaying.get()) {
            stopMusic(musicInfo.id);
        } else {
            startMusic(musicInfo);
        }
    }

    private void startMusic(MusicPanelState.MusicInfo musicInfo) {
        LiveKitLog.info(TAG + "[" + mRoomId + "] startMusic:[musicInfo:" + musicInfo + "]");
        int id = musicInfo.id;
        String path = musicInfo.path;
        TXAudioEffectManager.AudioMusicParam audioMusicParam = new TXAudioEffectManager.AudioMusicParam(id, path);
        audioMusicParam.loopCount = Integer.MAX_VALUE;
        audioMusicParam.publish = true;
        mTRTCCloud.getAudioEffectManager().startPlayMusic(audioMusicParam);
        mTRTCCloud.getAudioEffectManager().setMusicPitch(id, musicInfo.pitch.get());
        mMusicPanelState.currentMusicInfo.get().isPlaying.set(true);
    }

    private void stopMusic(int id) {
        LiveKitLog.info(TAG + "[" + mRoomId + "] stopMusic:[id:" + id + "]");
        mTRTCCloud.getAudioEffectManager().stopPlayMusic(id);
        mMusicPanelState.currentMusicInfo.get().isPlaying.set(false);
    }

    public void deleteMusic(MusicPanelState.MusicInfo musicInfo) {
        boolean isPlaying = musicInfo.isPlaying.get();
        LiveKitLog.info(TAG + "[" + mRoomId + "] deleteMusic:[musicInfo:" + musicInfo
                + ",isPlaying:" + isPlaying + "]");
        if (isPlaying) {
            stopMusic(musicInfo.id);
        }
        mMusicPanelState.currentMusicInfo.set(new MusicPanelState.MusicInfo());
        mMusicPanelState.musicList.remove(musicInfo);
    }
}
