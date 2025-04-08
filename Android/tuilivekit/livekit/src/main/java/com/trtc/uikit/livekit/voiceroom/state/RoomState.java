package com.trtc.uikit.livekit.voiceroom.state;

import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.manager.api.Constants;

import java.util.HashSet;
import java.util.Set;

public class RoomState {
    public String                                  roomId        = "";
    public long                                    createTime    = 0;
    public UserState.UserInfo                      ownerInfo     = new UserState.UserInfo();
    public MutableLiveData<String>                 roomName      = new MutableLiveData<>("");
    public MutableLiveData<String>                 coverURL      = new MutableLiveData<>(Constants.DEFAULT_COVER_URL);
    public MutableLiveData<String>                 backgroundURL = new MutableLiveData<>(Constants.DEFAULT_BACKGROUND_URL);
    public MutableLiveData<TUIRoomDefine.SeatMode> seatMode      =
            new MutableLiveData<>(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
    public MutableLiveData<Integer>                userCount     = new MutableLiveData<>(0);
    public MutableLiveData<Integer>                maxSeatCount  = new MutableLiveData<>(0);
    public LiveExtraInfo                           liveExtraInfo = new LiveExtraInfo();
    public MutableLiveData<LiveStatus>             liveStatus    = new MutableLiveData<>(LiveStatus.NONE);

    public void reset() {
        createTime = 0;
        roomName.setValue("");
        coverURL.setValue(Constants.DEFAULT_COVER_URL);
        backgroundURL.setValue(Constants.DEFAULT_COVER_URL);
        seatMode.setValue(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
        userCount.setValue(0);
        maxSeatCount.setValue(0);
        liveExtraInfo = new LiveExtraInfo();
    }

    public void updateState(TUIRoomDefine.RoomInfo roomInfo) {
        this.roomId = roomInfo.roomId;
        if (roomInfo.createTime != 0) {
            this.createTime = roomInfo.createTime;
        } else {
            this.createTime = System.currentTimeMillis();
        }
        this.roomName.setValue(roomInfo.name);
        this.seatMode.setValue(roomInfo.seatMode);
        this.ownerInfo.userId = roomInfo.ownerId;
        this.maxSeatCount.setValue(roomInfo.maxSeatCount);
    }

    public enum LiveStatus {
        NONE,
        PREVIEWING,
        PUSHING,
        PLAYING,
        DASHBOARD
    }

    public enum LiveStreamPrivacyStatus {
        PUBLIC(R.string.common_stream_privacy_status_default),
        PRIVACY(R.string.common_stream_privacy_status_privacy);

        public final int resId;

        LiveStreamPrivacyStatus(Integer id) {
            this.resId = id;
        }
    }

    public enum LiveCategory {
        DAILY_CHAT(0),
        APPEARANCE(1),
        KNOWLEDGE_TEACHING(2),
        SHOPPING(3),
        MUSIC(4);

        public final int id;

        LiveCategory(int id) {
            this.id = id;
        }

        public static LiveCategory getCategory(int id) {
            for (LiveCategory category : LiveCategory.values()) {
                if (category.id == id) {
                    return category;
                }
            }
            return null;
        }
    }

    public static class LiveExtraInfo {
        public MutableLiveData<LiveStreamPrivacyStatus> liveMode         = new MutableLiveData<>(LiveStreamPrivacyStatus.PUBLIC);
        public int                                      maxAudienceCount = 0;
        public int                                      messageCount     = 0;
        public int                                      giftIncome       = 0;
        public Set<String>                              giftPeopleSet    = new HashSet<>();
        public int                                      likeCount        = 0;
    }
}
