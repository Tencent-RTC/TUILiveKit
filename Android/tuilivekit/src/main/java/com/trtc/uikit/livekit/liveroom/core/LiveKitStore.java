package com.trtc.uikit.livekit.liveroom.core;

import com.tencent.qcloud.tuicore.TUILogin;
import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.liveroom.data.LiveRoomInfo;
import com.trtc.uikit.livekit.liveroom.data.MusicInfo;
import com.trtc.uikit.livekit.liveroom.data.UserInfo;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

public class LiveKitStore {

    private static volatile LiveKitStore sInstance;

    public UserInfo                          selfInfo              = new UserInfo(TUILogin.getUserId());
    public LiveRoomInfo                      selfRoomInfo          = new LiveRoomInfo(TUILogin.getUserId());
    public LiveData<LinkedHashSet<UserInfo>> applyLinkAudienceList = new LiveData<>(new LinkedHashSet<>());
    public List<MusicInfo>                   musicList             = new ArrayList<>();
    public LiveData<MusicInfo>               currentMusicInfo      = new LiveData<>(new MusicInfo());
    public LiveData<Boolean>                 isPortrait            = new LiveData<>(true);

    private LiveKitStore() {
    }

    public static LiveKitStore sharedInstance() {
        if (sInstance == null) {
            synchronized (LiveKitStore.class) {
                if (sInstance == null) {
                    sInstance = new LiveKitStore();
                }
            }
        }
        return sInstance;
    }

    public static void destroyInstance() {
        if (sInstance != null) {
            sInstance.destroy();
        }
    }

    public void setSelfStatus(TUILiveDefine.UserInteractionStatus status) {
        selfInfo.status.set(status);
    }

    private void destroy() {
        selfInfo.role.set(TUILiveDefine.RoleType.AUDIENCE);
        selfInfo.status.set(TUILiveDefine.UserInteractionStatus.NONE);
        selfInfo.requestId = "";

        selfRoomInfo = new LiveRoomInfo(TUILogin.getUserId());
        applyLinkAudienceList = new LiveData<>(new LinkedHashSet<>());
        currentMusicInfo = new LiveData<>(new MusicInfo());
        isPortrait = new LiveData<>(true);
        musicList = new ArrayList<>();
    }
}
