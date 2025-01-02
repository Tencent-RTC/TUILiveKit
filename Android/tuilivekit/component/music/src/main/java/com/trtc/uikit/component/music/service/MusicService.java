package com.trtc.uikit.component.music.service;

import android.util.Log;

import com.tencent.cloud.tuikit.engine.room.TUIRoomEngine;
import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.trtc.TRTCCloud;
import com.trtc.uikit.component.common.StateCache;
import com.trtc.uikit.component.music.store.MusicPanelState;

import java.util.HashMap;
import java.util.Map;

public class MusicService {
    public static final  String TAG       = "MusicService";
    private static final String STATE_KEY = "key_state_music_panel";

    private final String          mRoomId;
    private final TRTCCloud       mTRTCCloud;
    public        MusicPanelState mMusicPanelState;

    public MusicService(String roomId) {
        mRoomId = roomId;
        mTRTCCloud = TUIRoomEngine.sharedInstance().getTRTCCloud();
        mMusicPanelState = getState();
    }

    private MusicPanelState getState() {
        StateCache stateCache = StateCache.getInstance();
        Map<String, Object> map = stateCache.get(mRoomId);
        if (map == null) {
            map = new HashMap<>();
            stateCache.set(mRoomId, map);
        }
        MusicPanelState state = (MusicPanelState) map.get(STATE_KEY);
        if (state == null) {
            state = new MusicPanelState();
            map.put(STATE_KEY, state);
        }
        return state;
    }

    public void operatePlayMusic(MusicPanelState.MusicInfo musicInfo) {
        MusicPanelState.MusicInfo currentMusicInfo = mMusicPanelState.currentMusicInfo.get();
        if (currentMusicInfo.id != MusicPanelState.MusicInfo.INVALID_ID && currentMusicInfo.id != musicInfo.id) {
            stopMusic(currentMusicInfo.id);
        }
        mMusicPanelState.currentMusicInfo.set(musicInfo);
        boolean isPlaying = musicInfo.isPlaying.get();
        Log.i(TAG, "operatePlayMusic:[isPlaying:" + isPlaying + "]");
        if (musicInfo.isPlaying.get()) {
            stopMusic(musicInfo.id);
        } else {
            startMusic(musicInfo);
        }
    }

    private void startMusic(MusicPanelState.MusicInfo musicInfo) {
        Log.i(TAG, "[" + mRoomId + "] startMusic:[musicInfo:" + musicInfo + "]");
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
        Log.i(TAG, "[" + mRoomId + "] stopMusic:[id:" + id + "]");
        mTRTCCloud.getAudioEffectManager().stopPlayMusic(id);
        mMusicPanelState.currentMusicInfo.get().isPlaying.set(false);
    }

    public void deleteMusic(MusicPanelState.MusicInfo musicInfo) {
        boolean isPlaying = musicInfo.isPlaying.get();
        Log.i(TAG, "[" + mRoomId + "] deleteMusic:[musicInfo:" + musicInfo
                + ",isPlaying:" + isPlaying + "]");
        if (isPlaying) {
            stopMusic(musicInfo.id);
        }
        mMusicPanelState.currentMusicInfo.set(new MusicPanelState.MusicInfo());
        mMusicPanelState.musicList.remove(musicInfo);
    }
}
