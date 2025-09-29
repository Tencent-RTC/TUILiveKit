import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../live_define.dart';

class LSRoomState {
  String roomId = '';
  TUILiveInfo liveInfo = TUILiveInfo();
  int createTime = 0;
  String roomName = '';
  final ValueNotifier<String> coverUrl = ValueNotifier('');
  int userCount = 0;
  final ValueNotifier<LiveStatus> liveStatus = ValueNotifier(LiveStatus.none);
  LiveExtraInfo liveExtraInfo = LiveExtraInfo();
  final ValueNotifier<bool> roomVideoStreamIsLandscape = ValueNotifier(false);
}

class LiveExtraInfo {
  LiveStreamPrivacyStatus liveMode = LiveStreamPrivacyStatus.public;
  int maxAudienceCount = 0;
  int messageCount = 0;
  int giftIncome = 0;
  Set<String> giftPeopleSet = {};
  int likeCount = 0;
  int activeStatus = 0;
}
