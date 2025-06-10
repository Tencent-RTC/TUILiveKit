package com.trtc.uikit.livekit.livestream.state;

import androidx.lifecycle.MutableLiveData;

public class MediaState {
    public MutableLiveData<Boolean> isAudioLocked       = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isVideoLocked       = new MutableLiveData<>(false);
    public VideoAdvanceSetting      videoAdvanceSetting = new VideoAdvanceSetting();

    public void reset() {
    }

    public static class VideoAdvanceSetting {
        public boolean isVisible         = false;
        public boolean isUltimateEnabled = false;
        public boolean isH265Enabled     = false;
        public boolean isBFrameEnabled   = false;
    }
}
