class Constants {
  static const String pluginName = 'tencent_live_uikit';
  static const String defaultCoverUrl =
      "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png";
  static const List<String> coverUrlList = [
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
    "https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover12.png",
  ];

  static const String giftViewType = "gift_view_type";
  static const String giftName = "gift_name";
  static const String giftCount = "gift_count";
  static const String giftIconUrl = "gift_icon_url";
  static const String giftReceiverUsername = "gift_receiver_username";
  static const int giftViewType1 = 1;

  static const String eventKeyLiveKit = "EVENT_KEY_LIVE_KIT";
  static const String eventSubKeyStartLiveRoom = "EVENT_SUB_KEY_START_LIVE_ROOM";
  static const String eventSubKeyStartVoiceRoom = "EVENT_SUB_KEY_START_VOICE_ROOM";
  static const String eventSubKeyCloseLiveRoom = "EVENT_SUB_KEY_CLOSE_LIVE_ROOM";
  static const String eventSubKeyLinkStatusChange = "EVENT_SUB_KEY_LINK_STATUS_CHANGE";
  static const String eventParamsKeyEnableSlide = "EVENT_PARAMS_KEY_ENABLE_SLIDE";

  static const int defaultMaxSeatCount = 8;
  static const int roomMaxShowUserCount = 100;

  static const int dataReportComponentLiveRoom = 21;
  static const int dataReportComponentVoiceRoom = 22;
  static const int dataReportFramework = 1;
  static const int dataReportLanguageFlutter = 9;
  static int dataReportComponent = dataReportComponentLiveRoom;
}
