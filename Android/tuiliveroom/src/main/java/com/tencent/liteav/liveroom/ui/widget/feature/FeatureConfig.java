package com.tencent.liteav.liveroom.ui.widget.feature;

import com.tencent.trtc.TRTCCloudDef;

public class FeatureConfig {
    public static final String AUDIO_EVALUATION_CHANGED = "AUDIO_EVALUATION_CHANGED";

    static final int DEFAULT_BITRATE = 3500;
    static final int DEFAULT_FPS     = 24;


    private int mVideoResolution = TRTCCloudDef.TRTC_VIDEO_RESOLUTION_1920_1080;

    private           int     mVideoFps              = DEFAULT_FPS;
    private           int     mVideoBitrate          = DEFAULT_BITRATE;
    private           boolean mIsMirror              = true;
    private           int     mMicVolume             = 100;
    private           int     mPlayoutVolume         = 100;
    private           boolean mAudioVolumeEvaluation = true;
    private transient boolean mRecording             = false;
    private           String  mPlayUrl;

    public static FeatureConfig getInstance() {
        return SingletonHolder.instance;
    }

    public int getVideoResolution() {
        return mVideoResolution;
    }

    public void setVideoResolution(int videoResolution) {
        mVideoResolution = videoResolution;
    }

    public int getVideoFps() {
        return mVideoFps;
    }

    public void setVideoFps(int videoFps) {
        mVideoFps = videoFps;
    }

    public int getVideoBitrate() {
        return mVideoBitrate;
    }

    public void setVideoBitrate(int videoBitrate) {
        mVideoBitrate = videoBitrate;
    }

    public boolean isMirror() {
        return mIsMirror;
    }

    public void setMirror(boolean mirror) {
        mIsMirror = mirror;
    }

    public int getMicVolume() {
        return mMicVolume;
    }

    public void setMicVolume(int micVolume) {
        mMicVolume = micVolume;
    }

    public int getPlayoutVolume() {
        return mPlayoutVolume;
    }

    public void setPlayoutVolume(int playoutVolume) {
        mPlayoutVolume = playoutVolume;
    }

    public boolean isAudioVolumeEvaluation() {
        return mAudioVolumeEvaluation;
    }

    public void setAudioVolumeEvaluation(boolean audioVolumeEvaluation) {
        mAudioVolumeEvaluation = audioVolumeEvaluation;
    }

    public boolean isRecording() {
        return mRecording;
    }

    public void setRecording(boolean recording) {
        mRecording = recording;
    }

    public String getPlayUrl() {
        return mPlayUrl;
    }

    public void setPlayUrl(String playUrl) {
        mPlayUrl = playUrl;
    }

    private static class SingletonHolder {
        private static FeatureConfig instance = new FeatureConfig();
    }
}
