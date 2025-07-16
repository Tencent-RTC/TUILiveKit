package com.trtc.uikit.livekit.features.anchorboardcast.state;

import android.graphics.Bitmap;

import androidx.lifecycle.MutableLiveData;

public class MediaState {
    public MutableLiveData<Boolean> isAudioLocked    = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isVideoLocked    = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> isPipModeEnabled = new MutableLiveData<>(false);
    public Bitmap                   bigMuteBitmap    = null;
    public Bitmap                   smallMuteBitmap  = null;
    public boolean                  isCameraOccupied = false;
}
