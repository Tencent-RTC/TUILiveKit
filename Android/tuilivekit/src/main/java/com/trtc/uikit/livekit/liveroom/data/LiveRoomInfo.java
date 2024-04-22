package com.trtc.uikit.livekit.liveroom.data;

import static com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.InteractionType.BROADCAST;

import androidx.annotation.Nullable;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.common.utils.Constants;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.InteractionType;
import com.trtc.uikit.livekit.liveroom.core.TUILiveDefine.UserLiveStatus;

import java.util.Objects;
import java.util.concurrent.CopyOnWriteArraySet;

public class LiveRoomInfo {
    public String roomId;

    public RoomConfig getRoomConfig() {
        return roomConfig;
    }

    public LiveData<String>                             name                = new LiveData<>("");
    public LiveData<String>                             coverURL            =
            new LiveData<>(Constants.DEFAULT_COVER_URL);
    public LiveData<String>                             category            = new LiveData<>("");
    public LiveData<LiveDefine.LiveStreamPrivacyStatus> liveMode            =
            new LiveData<>(LiveDefine.LiveStreamPrivacyStatus.PUBLIC);
    public LiveData<InteractionType>                    interactionType     = new LiveData<>(BROADCAST);
    public UserInfo                                     anchorInfo          = new UserInfo();
    public LiveData<CopyOnWriteArraySet<UserInfo>>      audienceList        =
            new LiveData<>(new CopyOnWriteArraySet<>());
    public LiveData<Integer>                            audienceCount       = new LiveData<>(0);
    public LiveData<CopyOnWriteArraySet<UserInfo>>      linkingAudienceList =
            new LiveData<>(new CopyOnWriteArraySet<>());
    public LiveData<UserLiveStatus>                     userLiveStatus      = new LiveData<>(UserLiveStatus.NONE);
    public RoomConfig                                   roomConfig          = new RoomConfig();
    public CopyOnWriteArraySet<String>                  hasVideoList        = new CopyOnWriteArraySet<>();
    public CopyOnWriteArraySet<String>                  hasAudioList        = new CopyOnWriteArraySet<>();

    public LiveRoomInfo(String roomId) {
        this.roomId = roomId;
    }

    @Override
    public boolean equals(@Nullable Object obj) {
        if (obj instanceof LiveRoomInfo) {
            return this.roomId.equals(((LiveRoomInfo) obj).roomId);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(roomId);
    }
}
