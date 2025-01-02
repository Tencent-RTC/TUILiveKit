package com.trtc.uikit.component.dashboard.store;

public class StreamDashboardUserState {
    public String  userId          = "";
    public boolean isLocal         = false;
    public String  videoResolution = "";
    public int     videoFrameRate  = 0;
    public int     videoBitrate    = 0;
    public int     audioSampleRate = 0;
    public int     audioBitrate    = 0;

    public StreamDashboardUserState(String userId, boolean isLocal, String videoResolution, int videoFrameRate,
                                    int videoBitrate, int audioSampleRate, int audioBitrate) {
        this.userId = userId;
        this.isLocal = isLocal;
        this.videoResolution = videoResolution;
        this.videoFrameRate = videoFrameRate;
        this.videoBitrate = videoBitrate;
        this.audioSampleRate = audioSampleRate;
        this.audioBitrate = audioBitrate;
    }
}
