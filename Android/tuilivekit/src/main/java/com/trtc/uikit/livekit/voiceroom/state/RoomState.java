package com.trtc.uikit.livekit.voiceroom.state;

import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.voiceroom.api.Constants;

import java.util.HashSet;
import java.util.Set;

public class RoomState {
    public String                           roomId        = "";
    public long                             createTime    = 0;
    public UserState.UserInfo               ownerInfo     = new UserState.UserInfo();
    public LiveData<String>                 roomName      = new LiveData<>("");
    public LiveData<String>                 coverURL      = new LiveData<>(Constants.DEFAULT_COVER_URL);
    public LiveData<String>                 backgroundURL = new LiveData<>(Constants.DEFAULT_BACKGROUND_URL);
    public LiveData<TUIRoomDefine.SeatMode> seatMode      =
            new LiveData<>(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
    public LiveData<Integer>                userCount     = new LiveData<>(0);
    public LiveData<Integer>                maxSeatCount  = new LiveData<>(0);
    public LiveExtraInfo                    liveExtraInfo = new LiveExtraInfo();
    public LiveData<LiveStatus>             liveStatus    = new LiveData<>(LiveStatus.NONE);

    public void reset() {
        createTime = 0;
        roomName.set("");
        coverURL.set(Constants.DEFAULT_COVER_URL);
        backgroundURL.set(Constants.DEFAULT_COVER_URL);
        seatMode.set(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
        userCount.set(0);
        maxSeatCount.set(0);
        liveExtraInfo = new LiveExtraInfo();
    }

    public void updateState(TUIRoomDefine.RoomInfo roomInfo) {
        this.roomId = roomInfo.roomId;
        this.createTime = roomInfo.createTime;
        this.roomName.set(roomInfo.name, false);
        this.seatMode.set(roomInfo.seatMode, false);
        this.ownerInfo.userId = roomInfo.ownerId;
        this.maxSeatCount.set(roomInfo.maxSeatCount, false);
    }

    public enum LiveStatus {
        NONE,
        PREVIEWING,
        PUSHING,
        PLAYING,
        DASHBOARD
    }

    public enum LiveStreamPrivacyStatus {
        PUBLIC(R.string.livekit_stream_privacy_status_default),
        PRIVACY(R.string.livekit_stream_privacy_status_privacy);

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
        public LiveData<String>                  category         = new LiveData<>("");
        public LiveData<LiveStreamPrivacyStatus> liveMode         = new LiveData<>(LiveStreamPrivacyStatus.PUBLIC);
        public int                               maxAudienceCount = 0;
        public int                               messageCount     = 0;
        public int                               giftIncome       = 0;
        public Set<String>                       giftPeopleSet    = new HashSet<>();
        public int                               likeCount        = 0;
    }
}
