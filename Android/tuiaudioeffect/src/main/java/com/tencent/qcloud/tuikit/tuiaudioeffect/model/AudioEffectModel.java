package com.tencent.qcloud.tuikit.tuiaudioeffect.model;

import android.content.Context;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.qcloud.tuicore.util.BackgroundTasks;
import com.tencent.qcloud.tuikit.tuiaudioeffect.R;
import com.tencent.qcloud.tuikit.tuiaudioeffect.util.AudioEffectUtils;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

/**
 * TUIAudioEffect组件的Model层实现类
 */
public class AudioEffectModel implements IAudioEffectModel {

    private final Context mContext;

    private TXAudioEffectManager mAudioEffectManager; // 音效管理器
    private boolean              mIsEnableVoiceEarMonitor = false;  // 是否开启耳返

    public AudioEffectModel(Context context) {
        this.mContext = context;
    }

    @Override
    public List<VoiceItemEntity> getVoiceReverbData() {
        List<VoiceItemEntity> list = new ArrayList<>();
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_reverbtype_origin),
                R.drawable.tuiaudioeffect_no_select_normal, R.drawable.tuiaudioeffect_no_select_hover,
                AudioEffectUtils.AUDIO_REVERB_TYPE_0));
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_reverbtype_ktv),
                R.drawable.tuiaudioeffect_reverbtype_ktv_normal, R.drawable.tuiaudioeffect_reverbtype_ktv_hover,
                AudioEffectUtils.AUDIO_REVERB_TYPE_1));
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_reverbtype_lowdeep),
                R.drawable.tuiaudioeffect_reverbtype_lowdeep_normal, R.drawable.tuiaudioeffect_reverbtype_lowdeep_hover,
                AudioEffectUtils.AUDIO_REVERB_TYPE_4));
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_reverbtype_hongliang),
                R.drawable.tuiaudioeffect_reverbtype_heavymetal_normal,
                R.drawable.tuiaudioeffect_reverbtype_heavymetal_hover,
                AudioEffectUtils.AUDIO_REVERB_TYPE_5));
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_reverbtype_heavymetal),
                R.drawable.tuiaudioeffect_reverbtype_hongliang_normal,
                R.drawable.tuiaudioeffect_reverbtype_hongliang_hover,
                AudioEffectUtils.AUDIO_REVERB_TYPE_6));
        return list;
    }

    @Override
    public List<VoiceItemEntity> getVoiceChangeData() {
        List<VoiceItemEntity> list = new ArrayList<>();
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_changetype_original),
                R.drawable.tuiaudioeffect_no_select_normal, R.drawable.tuiaudioeffect_no_select_hover,
                AudioEffectUtils.AUDIO_VOICECHANGER_TYPE_0));
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_changetype_child),
                R.drawable.tuiaudioeffect_changetype_child_normal, R.drawable.tuiaudioeffect_changetype_child_hover,
                AudioEffectUtils.AUDIO_VOICECHANGER_TYPE_1));
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_changetype_luoli),
                R.drawable.tuiaudioeffect_changetype_luoli_normal, R.drawable.tuiaudioeffect_changetype_luoli_hover,
                AudioEffectUtils.AUDIO_VOICECHANGER_TYPE_2));
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_changetype_dashu),
                R.drawable.tuiaudioeffect_changetype_dashu_normal, R.drawable.tuiaudioeffect_changetype_dashu_hover,
                AudioEffectUtils.AUDIO_VOICECHANGER_TYPE_3));
        list.add(new VoiceItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_changetype_kongling),
                R.drawable.tuiaudioeffect_reverbtype_kongling_normal,
                R.drawable.tuiaudioeffect_reverbtype_kongling_hover,
                AudioEffectUtils.AUDIO_VOICECHANGER_TYPE_11));
        return list;
    }

    @Override
    public List<BGMItemEntity> getSongData() {
        List<BGMItemEntity> list = new ArrayList<>();
        list.add(new BGMItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_bg_music_positive_happy),
                AudioEffectUtils.ONLINE_BGM_FIRST));
        list.add(new BGMItemEntity(mContext.getResources().getString(
                R.string.tuiaudioeffect_bg_music_sad_cinematic_piano),
                AudioEffectUtils.ONLINE_BGM_SECOND));
        list.add(new BGMItemEntity(mContext.getResources().getString(R.string.tuiaudioeffect_bg_music_wonder_world),
                AudioEffectUtils.ONLINE_BGM_THIRD));
        return list;
    }

    public void setAudioEffectManager(TXAudioEffectManager mAudioEffectManager) {
        this.mAudioEffectManager = mAudioEffectManager;
    }

    @Override
    public void setMusicPublishVolume(int id, int volume) {
        if (null != mAudioEffectManager && -1 != id) {
            mAudioEffectManager.setMusicPublishVolume(id, volume);
        }
    }

    @Override
    public void setMusicPlayoutVolume(int id, int volume) {
        if (null != mAudioEffectManager && -1 != id) {
            mAudioEffectManager.setMusicPlayoutVolume(id, volume);
        }
    }

    @Override
    public void setVoiceCaptureVolume(int volume) {
        if (null != mAudioEffectManager) {
            mAudioEffectManager.setVoiceCaptureVolume(volume);
        }
    }

    @Override
    public void setMusicPitch(int id, float pitch) {
        if (null != mAudioEffectManager) {
            mAudioEffectManager.setMusicPitch(id, pitch);
        }
    }

    @Override
    public void setVoiceChangerType(TXAudioEffectManager.TXVoiceChangerType type) {
        if (null != mAudioEffectManager) {
            mAudioEffectManager.setVoiceChangerType(type);
        }
    }

    @Override
    public void setVoiceReverbType(TXAudioEffectManager.TXVoiceReverbType type) {
        if (null != mAudioEffectManager) {
            mAudioEffectManager.setVoiceReverbType(type);
        }
    }

    @Override
    public void startPlayMusic(TXAudioEffectManager.AudioMusicParam audioMusicParam) {
        if (null != mAudioEffectManager) {
            mAudioEffectManager.startPlayMusic(audioMusicParam);
        }
    }

    @Override
    public void stopPlayMusic(int id) {
        if (null != mAudioEffectManager) {
            mAudioEffectManager.stopPlayMusic(id);
        }
    }

    @Override
    public void pausePlayMusic(int id) {
        if (null != mAudioEffectManager) {
            mAudioEffectManager.pausePlayMusic(id);
        }
    }

    @Override
    public void resumePlayMusic(int id) {
        if (null != mAudioEffectManager) {
            mAudioEffectManager.resumePlayMusic(id);
        }
    }

    @Override
    public void getMusicDurationInMS(String path, GetMusicDurationCallback callback) {
        final TXAudioEffectManager audioEffectManager = mAudioEffectManager;
        if (audioEffectManager == null) {
            return;
        }
        final WeakReference<GetMusicDurationCallback> callbackWeakReference = new WeakReference<>(callback);
        new Thread(() -> {
            long duration = audioEffectManager.getMusicDurationInMS(path);
            BackgroundTasks.getInstance().runOnUiThread(() -> {
                GetMusicDurationCallback durationCallback = callbackWeakReference.get();
                if (durationCallback == null) {
                    return;
                }
                if (duration < 0) {
                    int errorCode = (int) duration;
                    durationCallback.onError(path, errorCode);
                } else {
                    durationCallback.onSuccess(path, duration);
                }
            });
        }).start();
    }

    @Override
    public void setMusicObserver(int id, TXAudioEffectManager.TXMusicPlayObserver observer) {
        if (null != mAudioEffectManager) {
            mAudioEffectManager.setMusicObserver(id, observer);
        }
    }

    @Override
    public void enableVoiceEarMonitor(boolean enable) {
        if (null != mAudioEffectManager) {
            mIsEnableVoiceEarMonitor = enable;
            mAudioEffectManager.enableVoiceEarMonitor(enable);
        }
    }

    @Override
    public boolean isEnableVoiceEarMonitor() {
        return mIsEnableVoiceEarMonitor;
    }

    public interface GetMusicDurationCallback {
        void onSuccess(String path, long duration);

        void onError(String path, int errorCode);
    }
}
