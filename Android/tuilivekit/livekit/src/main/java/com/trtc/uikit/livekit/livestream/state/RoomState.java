package com.trtc.uikit.livekit.livestream.state;

import static com.trtc.uikit.livekit.livestream.manager.Constants.DEFAULT_BACKGROUND_URL;
import static com.trtc.uikit.livekit.livestream.manager.Constants.DEFAULT_COVER_URL;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.NONE;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStreamPrivacyStatus.PUBLIC;

import com.trtc.tuikit.common.livedata.LiveData;
import com.trtc.uikit.livekit.R;

public class RoomState {
    public String                            roomId           = "";
    public long                              createTime       = 0;
    public UserState.UserInfo                ownerInfo        = new UserState.UserInfo();
    public LiveData<String>                  roomName         = new LiveData<>("");
    public LiveData<String>                  coverURL         = new LiveData<>(DEFAULT_COVER_URL);
    public LiveData<String>                  backgroundURL    = new LiveData<>(DEFAULT_BACKGROUND_URL);
    public LiveData<Integer>                 userCount        = new LiveData<>(0);
    public LiveData<Integer>                 maxSeatCount     = new LiveData<>(0);
    public LiveData<LiveStatus>              liveStatus       = new LiveData<>(NONE);
    public LiveData<LiveStreamPrivacyStatus> liveMode         = new LiveData<>(PUBLIC);
    public LiveData<String>                  category         = new LiveData<>("");
    public LiveData<Integer>                 activityStatus   = new LiveData<>(0);
    public int                               maxAudienceCount = 0;

    public void reset() {
        createTime = 0;
        roomName.set("");
        coverURL.set(DEFAULT_COVER_URL);
        backgroundURL.set(DEFAULT_COVER_URL);
        userCount.set(0);
        maxSeatCount.set(0);
        liveStatus.set(NONE);
        liveMode.set(PUBLIC);
        category.set("");
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

}
