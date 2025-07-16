package com.trtc.uikit.livekit.features.audiencecontainer.state;

import android.graphics.Bitmap;

import androidx.lifecycle.MutableLiveData;

public class MediaState {
    public MutableLiveData<Boolean> isAudioLocked          = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isVideoLocked          = new MutableLiveData<>(false);
    public VideoAdvanceSetting      videoAdvanceSetting    = new VideoAdvanceSetting();
    public MutableLiveData<Boolean> isPictureInPictureMode = new MutableLiveData<>(false);
    public Bitmap                   bigMuteBitmap          = null;
    public Bitmap                   smallMuteBitmap        = null;

    public void reset() {
    }

    public static class VideoAdvanceSetting {
        public boolean isUltimateEnabled = false;
        public boolean isH265Enabled     = false;
        public boolean isBFrameEnabled   = false;
    }
}
