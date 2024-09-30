package com.trtc.uikit.livekit.common.utils;

public class Constants {
    public static final String DEFAULT_COVER_URL = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com"
            + "/voice_room/voice_room_cover1.png";

    public static final String[] COVER_URL_LIST = {
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

    public static final String DEFAULT_BACKGROUND_URL = "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com"
            + "/voice_room/voice_room_background1.png";

    public static final String[] BACKGROUND_URL_LIST = {
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background1.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background2.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background3.png"
    };

    public static final String[] BACKGROUND_THUMB_URL_LIST = {
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background1_thumb.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background2_thumb.png",
            "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background3_thumb.png"
    };

    public static final String GIFT_VIEW_TYPE         = "gift_view_type";
    public static final String GIFT_NAME              = "gift_name";
    public static final String GIFT_COUNT             = "gift_count";
    public static final String GIFT_ICON_URL          = "gift_icon_url";
    public static final String GIFT_RECEIVER_USERNAME = "gift_receiver_username";
    public static final int    GIFT_VIEW_TYPE_1       = 1;

    public static final String EVENT_KEY_LIVE_KIT                       = "EVENT_KEY_LIVE_KIT";
    public static final String EVENT_SUB_KEY_START_LIVE_ROOM            = "EVENT_SUB_KEY_START_LIVE_ROOM";
    public static final String EVENT_SUB_KEY_START_VOICE_ROOM           = "EVENT_SUB_KEY_START_VOICE_ROOM";
    public static final String EVENT_SUB_KEY_CLOSE_LIVE_ROOM            = "EVENT_SUB_KEY_CLOSE_LIVE_ROOM";
    public static final String EVENT_SUB_KEY_CLOSE_VOICE_ROOM           = "EVENT_SUB_KEY_CLOSE_VOICE_ROOM";
    public static final String EVENT_SUB_KEY_LINK_STATUS_CHANGE         = "EVENT_SUB_KEY_LINK_STATUS_CHANGE";
    public static final String EVENT_PARAMS_KEY_ENABLE_SLIDE            = "EVENT_PARAMS_KEY_ENABLE_SLIDE";
    public static final String EVENT_SUB_KEY_FINISH_ACTIVITY    = "EVENT_SUB_KEY_FINISH_ACTIVITY";
    public static final String EVENT_SUB_KEY_REQUEST_CONNECTION = "EVENT_SUB_KEY_REQUEST_CONNECTION_SUCCESS";
    public static final String EVENT_SUB_KEY_TOAST              = "EVENT_SUB_KEY_TOAST";

    public static final int DEFAULT_MAX_SEAT_COUNT   = 10;
    public static final int ROOM_MAX_SHOW_USER_COUNT = 100;

    public static final int DATA_REPORT_COMPONENT_LIVE_ROOM  = 21;
    public static final int DATA_REPORT_COMPONENT_VOICE_ROOM = 22;
    public static final int DATA_REPORT_FRAMEWORK            = 1;
    public static final int DATA_REPORT_LANGUAGE_JAVA        = 1;
    public static       int DATA_REPORT_COMPONENT            = DATA_REPORT_COMPONENT_LIVE_ROOM;
}
