package com.trtc.uikit.livekit.liveroom.data;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine.VideoQuality;
import com.trtc.tuikit.common.livedata.LiveData;

public class VideoInfo {
    public LiveData<VideoQuality> videoQuality   = new LiveData<>(VideoQuality.Q_1080P);
    public LiveData<Boolean>      isMirror       = new LiveData<>(true);
    public LiveData<Boolean>      isFrontCamera  = new LiveData<>(true);
    public LiveData<Boolean>      isCameraOpened = new LiveData<>(false);

}
