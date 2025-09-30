package com.trtc.uikit.livekit.features.audiencecontainer.state;

import android.graphics.Bitmap;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class MediaState {
    public MutableLiveData<Boolean>                          isAudioLocked          = new MutableLiveData<>(false);
    public MutableLiveData<Boolean>                          isVideoLocked          = new MutableLiveData<>(false);
    public VideoAdvanceSetting                               videoAdvanceSetting    = new VideoAdvanceSetting();
    public MutableLiveData<Boolean>                          isPictureInPictureMode = new MutableLiveData<>(false);
    public MutableLiveData<TUIRoomDefine.VideoQuality>       playbackQuality        = new MutableLiveData<>(null);
    public MutableLiveData<List<TUIRoomDefine.VideoQuality>> playbackQualityList    = new MutableLiveData<>(new ArrayList<>());
    public Bitmap                                            bigMuteBitmap          = null;
    public Bitmap                                            smallMuteBitmap        = null;

    public void reset() {
    }

    public static class VideoAdvanceSetting {
        public boolean isUltimateEnabled = false;
        public boolean isH265Enabled     = false;
        public boolean isBFrameEnabled   = false;
    }
}
