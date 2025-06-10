package com.trtc.uikit.livekit.features.anchorprepare.state;

import static com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine.LiveStreamPrivacyStatus.PUBLIC;

import androidx.lifecycle.MutableLiveData;

import com.trtc.uikit.livekit.features.anchorprepare.AnchorPrepareViewDefine.LiveStreamPrivacyStatus;

public class AnchorPrepareState {
    public static final String   DEFAULT_COVER_URL     = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com"
            + "/voice_room/voice_room_cover1.png";
    public static final int      MAX_INPUT_BYTE_LENGTH = 100;
    public static final String[] COVER_URL_LIST        = {
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover2.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover3.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover4.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover5.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover6.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover7.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover8.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover9.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover10.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover11.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover12.png",};

    public String                                   selfUserId;
    public String                                   selfUserName;
    public String                                   roomId;
    public MutableLiveData<Boolean>                 useFrontCamera = new MutableLiveData<>(true);
    public MutableLiveData<String>                  coverURL       = new MutableLiveData<>(DEFAULT_COVER_URL);
    public MutableLiveData<LiveStreamPrivacyStatus> liveMode       = new MutableLiveData<>(PUBLIC);
    public MutableLiveData<String>                  roomName       = new MutableLiveData<>("");
    public MutableLiveData<Boolean>                 startLiveClick = new MutableLiveData<>(false);
}
