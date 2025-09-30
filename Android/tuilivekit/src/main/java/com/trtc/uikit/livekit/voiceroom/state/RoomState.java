package com.trtc.uikit.livekit.voiceroom.state;

import static com.trtc.uikit.livekit.common.ConstantsKt.DEFAULT_BACKGROUND_URL;
import static com.trtc.uikit.livekit.common.ConstantsKt.DEFAULT_COVER_URL;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.tencent.cloud.tuikit.engine.extension.TUILiveListManager;
import com.tencent.cloud.tuikit.engine.room.TUIRoomDefine;
import com.trtc.uikit.livekit.R;

public class RoomState {
    public String                                  roomId        = "";
    public TUILiveListManager.LiveInfo             liveInfo      = new TUILiveListManager.LiveInfo();
    public long                                    createTime    = 0;
    public TUIRoomDefine.UserInfo                  ownerInfo     = new TUIRoomDefine.UserInfo();
    public MutableLiveData<String>                 roomName      = new MutableLiveData<>("");
    public MutableLiveData<String>                 coverURL      = new MutableLiveData<>(DEFAULT_COVER_URL);
    public MutableLiveData<String>                 backgroundURL =
            new MutableLiveData<>(DEFAULT_BACKGROUND_URL);
    public MutableLiveData<TUIRoomDefine.SeatMode> seatMode      =
            new MutableLiveData<>(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
    public MutableLiveData<Integer>                userCount     = new MutableLiveData<>(0);
    public MutableLiveData<Integer>                maxSeatCount  = new MutableLiveData<>(0);
    public LiveExtraInfo                           liveExtraInfo = new LiveExtraInfo();
    public MutableLiveData<LiveStatus>             liveStatus    = new MutableLiveData<>(LiveStatus.NONE);
    public MutableLiveData<LayoutType> layoutType = new MutableLiveData<>(LayoutType.VoiceRoom);

    public void reset() {
        createTime = 0;
        roomName.setValue("");
        coverURL.setValue(DEFAULT_COVER_URL);
        backgroundURL.setValue(DEFAULT_COVER_URL);
        seatMode.setValue(TUIRoomDefine.SeatMode.FREE_TO_TAKE);
        userCount.setValue(0);
        maxSeatCount.setValue(0);
        liveExtraInfo = new LiveExtraInfo();
    }

    public void updateState(TUILiveListManager.LiveInfo liveInfo) {
        this.roomId = liveInfo.roomId;
        if (liveInfo.createTime != 0) {
            this.createTime = liveInfo.createTime;
        } else {
            this.createTime = System.currentTimeMillis();
        }
        this.roomName.setValue(liveInfo.name);
        this.seatMode.setValue(liveInfo.seatMode);
        this.ownerInfo.userId = liveInfo.ownerId;
        this.maxSeatCount.setValue(liveInfo.maxSeatCount);
        this.liveInfo = liveInfo;
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

    public enum LayoutType {
        KTVRoom("KTVRoom"),
        VoiceRoom("ChatRoom");

        private final String desc;

        LayoutType(String desc) {
            this.desc = desc;
        }

        @NonNull
        @Override
        public String toString() {
            return desc;
        }
    }
    public static class LiveExtraInfo {
        public MutableLiveData<LiveStreamPrivacyStatus> liveMode =
                new MutableLiveData<>(LiveStreamPrivacyStatus.PUBLIC);

        public long maxAudienceCount = 0;
        public long messageCount     = 0;
        public long giftIncome       = 0;
        public long giftSenderCount  = 0;
        public long likeCount        = 0;
    }
}
