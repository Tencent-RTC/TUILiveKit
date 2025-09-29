import 'package:flutter/cupertino.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

class RoomState {
  String roomId = '';
  int createTime = 0;
  User ownerInfo = User();
  ValueNotifier<String> roomName = ValueNotifier('');
  ValueNotifier<String> coverUrl = ValueNotifier('https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_cover1.png');
  ValueNotifier<String> backgroundUrl = ValueNotifier('https://liteav-test-1252463788.cos.ap-guangzhou.myqcloud.com/voice_room/voice_room_background1.png');
  ValueNotifier<TUISeatMode> seatMode = ValueNotifier(TUISeatMode.applyToTake);
  ValueNotifier<int> userCount = ValueNotifier(0);
  ValueNotifier<int> maxSeatCount = ValueNotifier(0);
  ValueNotifier<LiveExtraInfo> liveExtraInfo = ValueNotifier(LiveExtraInfo());
  ValueNotifier<bool> exitRoom = ValueNotifier(false);
}

class LiveExtraInfo {
  final ValueNotifier<PrivacyStatus> liveMode = ValueNotifier(PrivacyStatus.publicity);
  int maxAudienceCount = 0;
  int messageCount = 0;
  int giftIncome = 0;
  int giftSenderCount = 0;
  int likeCount = 0;
}
