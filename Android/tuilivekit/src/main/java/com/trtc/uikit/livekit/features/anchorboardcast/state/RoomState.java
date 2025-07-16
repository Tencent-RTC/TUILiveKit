package com.trtc.uikit.livekit.features.anchorboardcast.state;

import static com.trtc.uikit.livekit.features.anchorboardcast.manager.Constants.DEFAULT_BACKGROUND_URL;
import static com.trtc.uikit.livekit.features.anchorboardcast.manager.Constants.DEFAULT_COVER_URL;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;

public class RoomState {
    public String                   roomId           = "";
    public long                     createTime       = 0;
    public MutableLiveData<String>  roomName         = new MutableLiveData<>("");
    public MutableLiveData<String>  coverURL         = new MutableLiveData<>(DEFAULT_COVER_URL);
    public MutableLiveData<String>  backgroundURL    = new MutableLiveData<>(DEFAULT_BACKGROUND_URL);
    public MutableLiveData<Integer> userCount        = new MutableLiveData<>(0);
    public MutableLiveData<Boolean> isPublicVisible  = new MutableLiveData<>(true);
    public MutableLiveData<Integer> activityStatus   = new MutableLiveData<>(0);
    public int                      maxAudienceCount = 0;
    public TUIRoomDefine.RoomInfo   roomInfo         = new TUIRoomDefine.RoomInfo();
}
