package com.trtc.uikit.livekit.liveroom.data;

import com.trtc.tuikit.common.livedata.LiveData;

public class RoomConfig {
    public LiveData<Boolean> enableLink           = new LiveData<>(true);
    public LiveData<Boolean> enablePK             = new LiveData<>(true);
    public LiveData<Boolean> enableGift           = new LiveData<>(true);
    public LiveData<Boolean> enableLike           = new LiveData<>(true);
    public LiveData<Boolean> enableChat           = new LiveData<>(true);
    public LiveData<Boolean> enableHiddenNickname = new LiveData<>(false);
    public LiveData<Boolean> enableShare          = new LiveData<>(false);
}
