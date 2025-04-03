import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/constants/constants.dart';

import '../index.dart';

class RoomState with ChangeNotifier {
  String roomId = "";
  int createTime = 0;
  final UserInfo ownerInfo = UserInfo();
  final ValueNotifier<String?> roomName = ValueNotifier("");
  final ValueNotifier<String> coverURL = ValueNotifier(Constants.defaultCoverUrl);
  final ValueNotifier<TUISeatMode> seatMode = ValueNotifier(TUISeatMode.freeToTake);
  final ValueNotifier<int> userCount = ValueNotifier(0);
  final ValueNotifier<int> maxSeatCount = ValueNotifier(0);
  final ValueNotifier<bool> enterRoomSuccess = ValueNotifier(false);
  LiveExtraInfo liveExtraInfo = LiveExtraInfo();

  void reset() {
    createTime = 0;
    roomName.value = '';
    coverURL.value = Constants.defaultCoverUrl;
    seatMode.value = TUISeatMode.freeToTake;
    userCount.value = 0;
    maxSeatCount.value = 0;
    liveExtraInfo = LiveExtraInfo();
    notifyListeners();
  }

  void updateState(TUIRoomInfo? roomInfo) {
    if (roomInfo == null) {
      return;
    }
    roomId = roomInfo.roomId;
    createTime = roomInfo.createTime;
    roomName.value = roomInfo.name;
    seatMode.value = roomInfo.seatMode;
    ownerInfo.userId = roomInfo.ownerId;
    ownerInfo.name.value = roomInfo.ownerName;
    ownerInfo.avatarUrl.value = roomInfo.ownerAvatarUrl;
    maxSeatCount.value = roomInfo.maxSeatCount;
    notifyListeners();
  }
}

class LiveExtraInfo {
  final ValueNotifier<LiveStreamPrivacyStatus> liveMode = ValueNotifier(LiveStreamPrivacyStatus.publicity);
  final Set<String> giftPeopleSet = <String>{};
  int maxAudienceCount = 0;
  int messageCount = 0;
  int giftIncome = 0;
  int likeCount = 0;
}
