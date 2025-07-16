import 'package:flutter/material.dart';

import '../live_define.dart';

class LSRoomState {
  String roomId = '';
  int createTime = 0;
  String roomName = '';
  final ValueNotifier<String> coverUrl = ValueNotifier('');
  int userCount = 0;
  final ValueNotifier<LiveStatus> liveStatus = ValueNotifier(LiveStatus.none);
  LiveExtraInfo liveExtraInfo = LiveExtraInfo();
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
