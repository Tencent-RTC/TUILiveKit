package com.tencent.qcloud.tuikit.tuiaudioeffect.presenter;

import androidx.annotation.NonNull;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.BGMItemEntity;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.IAudioEffectModel;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.VoiceItemEntity;
import com.tencent.qcloud.tuikit.tuiaudioeffect.util.AudioEffectUtils;

import java.util.List;

/**
 * TUIAudioEffect组件Presenter层实现类
 */
public class AudioEffectPresenter implements IAudioEffectPresenter {

    private final IAudioEffectModel mModel;

    public AudioEffectPresenter(@NonNull IAudioEffectModel model) {
        this.mModel = model;
    }

    @Override
    public List<VoiceItemEntity> getVoiceReverbData() {
        return mModel.getVoiceReverbData();
    }

    @Override
    public List<VoiceItemEntity> getVoiceChangeData() {
        return mModel.getVoiceChangeData();
    }

    @Override
    public List<BGMItemEntity> getSongData() {
        return mModel.getSongData();
    }

    @Override
    public void setAudioEffectManager(TXAudioEffectManager audioEffectManager) {
        mModel.setAudioEffectManager(audioEffectManager);
    }

    @Override
    public void setMusicPublishVolume(int id, int volume) {
        mModel.setMusicPublishVolume(id, volume);
    }

    @Override
    public void setMusicPlayoutVolume(int id, int volume) {
        mModel.setMusicPlayoutVolume(id, volume);
    }

    @Override
    public void setVoiceCaptureVolume(int volume) {
        mModel.setVoiceCaptureVolume(volume);
    }

    @Override
    public void setMusicPitch(int id, float pitch) {
        mModel.setMusicPitch(id, pitch);
    }

    @Override
    public void setVoiceChangerType(int type) {
        mModel.setVoiceChangerType(AudioEffectUtils.translateChangerType(type));
    }

    @Override
    public void setVoiceReverbType(int type) {
        mModel.setVoiceReverbType(AudioEffectUtils.translateReverbType(type));
    }

    @Override
    public void startPlayMusic(int id, String path, boolean publish) {
        final TXAudioEffectManager.AudioMusicParam audioMusicParam = new TXAudioEffectManager.AudioMusicParam(id, path);
        audioMusicParam.publish = publish; //上行
        mModel.startPlayMusic(audioMusicParam);
    }

    @Override
    public void stopPlayMusic(int id) {
        mModel.stopPlayMusic(id);
    }

    @Override
    public void pausePlayMusic(int id) {
        mModel.pausePlayMusic(id);
    }

    @Override
    public void resumePlayMusic(int id) {
        mModel.resumePlayMusic(id);
    }

    @Override
    public long getMusicDurationInMS(String path) {
        return mModel.getMusicDurationInMS(path);
    }

    @Override
    public void setMusicObserver(int id, final IMusicPlayObserver observer) {
        mModel.setMusicObserver(id, new TXAudioEffectManager.TXMusicPlayObserver() {
            @Override
            public void onStart(int i, int i1) {
                if (null != observer) {
                    observer.onStart(i, i1);
                }
            }

            @Override
            public void onPlayProgress(int i, long l, long l1) {
                if (null != observer) {
                    observer.onPlayProgress(i, l, l1);
                }
            }

            @Override
            public void onComplete(int i, int i1) {
                if (null != observer) {
                    observer.onComplete(i, i1);
                }
            }
        });
    }

    @Override
    public void enableVoiceEarMonitor(boolean enable) {
        mModel.enableVoiceEarMonitor(enable);
    }

    @Override
    public boolean isEnableVoiceEarMonitor() {
        return mModel.isEnableVoiceEarMonitor();
    }
}
