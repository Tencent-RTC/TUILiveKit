package com.trtc.uikit.livekit.component.pictureinpicture;

import androidx.lifecycle.MutableLiveData;

public class PictureInPictureState {
    public MutableLiveData<String> roomId = new MutableLiveData<>("");
    public boolean anchorIsPictureInPictureMode = false;
    public boolean audienceIsPictureInPictureMode = false;
}
