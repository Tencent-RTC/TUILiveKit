package com.trtc.uikit.livekit.features.anchorboardcast.state;

import static com.trtc.uikit.livekit.livestream.manager.Constants.DEFAULT_BACKGROUND_URL;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStreamPrivacyStatus.PUBLIC;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.extension.TUILiveConnectionManager.ConnectionUser;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.livestream.state.RoomState;
import com.trtc.uikit.livekit.livestream.state.UserState;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class AnchorState {
    public TUIRoomDefine.LoginUserInfo              loginUserInfo       = new TUIRoomDefine.LoginUserInfo();
    public String                                   roomId;
    public TUIRoomDefine.RoomInfo                   roomInfo;
    public TUIRoomDefine.UserInfo                   ownerInfo           = new TUIRoomDefine.UserInfo();
    public String                                   roomName;
    public long                                     createTime          = 0;
    public String                                   backgroundURL       = DEFAULT_BACKGROUND_URL;
    public RoomState.LiveStreamPrivacyStatus        liveMode            = PUBLIC;
    public MutableLiveData<Set<UserState.UserInfo>> myFollowingUserList = new MutableLiveData<>(new LinkedHashSet<>());
    public MutableLiveData<TUIRoomDefine.UserInfo>  enterUserInfo       = new MutableLiveData<>();
    public MutableLiveData<List<ConnectionUser>>    recommendUsers      = new MutableLiveData<>(new ArrayList<>());
    public String                                   recommendedCursor   = "";
}
