package com.trtc.uikit.livekit.livestream.state;

import static com.trtc.uikit.livekit.livestream.manager.Constants.DEFAULT_BACKGROUND_URL;
import static com.trtc.uikit.livekit.livestream.manager.Constants.DEFAULT_COVER_URL;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStatus.NONE;
import static com.trtc.uikit.livekit.livestream.state.RoomState.LiveStreamPrivacyStatus.PUBLIC;

import androidx.lifecycle.MutableLiveData;

import com.trtc.uikit.livekit.R;

public class RoomState {
    public String                                   roomId           = "";
    public long                                     createTime       = 0;
    public UserState.UserInfo                       ownerInfo        = new UserState.UserInfo();
    public MutableLiveData<String>                  roomName         = new MutableLiveData<>("");
    public MutableLiveData<String>                  coverURL         = new MutableLiveData<>(DEFAULT_COVER_URL);
    public MutableLiveData<String>                  backgroundURL    = new MutableLiveData<>(DEFAULT_BACKGROUND_URL);
    public MutableLiveData<Integer>                 userCount        = new MutableLiveData<>(0);
    public MutableLiveData<LiveStatus>              liveStatus       = new MutableLiveData<>(NONE);
    public MutableLiveData<LiveStreamPrivacyStatus> liveMode         = new MutableLiveData<>(PUBLIC);
    public MutableLiveData<Integer>                 activityStatus   = new MutableLiveData<>(0);
    public int                                      maxAudienceCount = 0;

    public void reset() {
        createTime = 0;
        roomName.setValue("");
        coverURL.setValue(DEFAULT_COVER_URL);
        backgroundURL.setValue(DEFAULT_COVER_URL);
        userCount.setValue(0);
        liveStatus.setValue(NONE);
        liveMode.setValue(PUBLIC);
    }

    public enum LiveStatus {
        NONE,
        PREVIEWING,
        PUSHING,
        PLAYING,
        DASHBOARD
    }

    public enum LiveStreamPrivacyStatus {
        PUBLIC(R.string.live_stream_privacy_status_default),
        PRIVACY(R.string.live_stream_privacy_status_privacy);

        public final int resId;

        LiveStreamPrivacyStatus(Integer id) {
            this.resId = id;
        }
    }
}
