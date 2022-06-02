package com.tencent.qcloud.tuikit.tuiaudioeffect.presenter;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.BGMItemEntity;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.VoiceItemEntity;

import java.util.List;

/**
 * TUIAudioEffect组件Presenter层接口
 */
public interface IAudioEffectPresenter {

    /**
     * 返回混响数据列表
     *
     * @return
     */
    List<VoiceItemEntity> getVoiceReverbData();

    /**
     * 返回变声数据列表
     *
     * @return
     */
    List<VoiceItemEntity> getVoiceChangeData();

    /**
     * 返回背景歌曲数据列表
     *
     * @return
     */
    List<BGMItemEntity> getSongData();

    /**
     * 设置TXAudioEffectManager对象
     *
     * @param audioEffectManager
     */
    void setAudioEffectManager(TXAudioEffectManager audioEffectManager);

    /**
     * 设置背景音乐的远端音量大小，即主播可以通过此接口设置远端观众能听到的背景音乐的音量大小。
     *
     * @param id     音乐 ID
     * @param volume 音量大小，100为原始音量，范围是：[0 ~ 150]，默认值为100
     */
    void setMusicPublishVolume(int id, int volume);

    /**
     * 设置背景音乐的本地音量大小，即主播可以通过此接口设置主播自己本地的背景音乐的音量大小。
     *
     * @param id     音乐 ID
     * @param volume 音量大小，100为原始音量，取值范围为[0 ~ 150]；默认值：100
     */
    void setMusicPlayoutVolume(int id, int volume);

    /**
     * 设置麦克风采集人声的音量
     *
     * @param volume 音量大小，100为原始音量，范围是：[0 ~ 150]，默认值为100
     */
    void setVoiceCaptureVolume(int volume);

    /**
     * 调整背景音乐的音调高低
     *
     * @param id    音乐 ID
     * @param pitch 音调，默认值是0.0f，范围是：[-1 ~ 1] 之间的浮点数；
     */
    void setMusicPitch(int id, float pitch);

    /**
     * 设置人声的变声特效（萝莉、大叔、重金属、外国人...）
     * 设置的效果在退房后会失效，如果下次进房还需要对应特效，需要调用此接口再次设置。
     *
     * @param type {@link com.tencent.qcloud.tuikit.tuiaudioeffect.util.AudioEffectUtils}
     */
    void setVoiceChangerType(int type);

    /**
     * 设置人声的混响效果（KTV、小房间、大会堂、低沉、洪亮...）
     * 注意：设置的效果在退房后会失效，如果下次进房还需要对应特效，需要调用此接口再次设置。
     *
     * @param type {@link com.tencent.qcloud.tuikit.tuiaudioeffect.util.AudioEffectUtils}
     */
    void setVoiceReverbType(int type);

    /**
     * 开始播放背景音乐
     * 每个音乐都需要您指定具体的 ID，您可以通过该 ID 对音乐的开始、停止、音量等进行设置。
     * 注意：若您想同时播放多个音乐，请分配不同的 ID 进行播放。 如果使用同一个 ID 播放不同音乐，SDK 会先停止播放旧的音乐，再播放新的音乐。
     *
     * @param id      音乐参数
     * @param path    音乐路径
     * @param publish 是否将音乐传到远端
     */
    void startPlayMusic(int id, String path, boolean publish);

    /**
     * 停止播放背景音乐
     *
     * @param id 音乐id
     */
    void stopPlayMusic(int id);

    /**
     * 暂停播放背景音乐
     *
     * @param id 音乐id
     */
    void pausePlayMusic(int id);

    /**
     * 恢复播放背景音乐
     *
     * @param id 音乐id
     */
    void resumePlayMusic(int id);

    /**
     * 获取景音乐文件的总时长（单位：毫秒）
     *
     * @param path 音乐文件路径，如果 path 为空，那么返回当前正在播放的 music 时长。
     * @return 成功返回时长，失败返回-1
     */
    long getMusicDurationInMS(String path);

    /**
     * 设置背景音乐的播放进度回调接口
     *
     * @param id       音乐id
     * @param observer 监听器 {@link IMusicPlayObserver}
     */
    void setMusicObserver(int id, IMusicPlayObserver observer);

    /**
     * 是否开启耳返
     * 开启后会在耳机里听到自己的声音。
     * 注意：仅在戴耳机时有效，暂时仅支持部分采集延迟较低的机型
     *
     * @param enable true：开启；false：关闭
     */
    void enableVoiceEarMonitor(boolean enable);

    /**
     * 判断是否支持耳返
     *
     * @return
     */
    boolean isEnableVoiceEarMonitor();

    interface IMusicPlayObserver {

        void onStart(int id, int errCode);

        void onPlayProgress(int id, long curPtsMS, long durationMS);

        void onComplete(int id, int errCode);
    }
}
