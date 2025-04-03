import 'package:flutter/cupertino.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

class SeatState {
  ValueNotifier<List<SeatInfo>> seatList = ValueNotifier([]);
  ValueNotifier<List<SeatApplication>> seatApplicationList = ValueNotifier([]);
  ValueNotifier<bool> isApplyingToTakeSeat = ValueNotifier(false);
  ValueNotifier<Set<String>> invitedUserIds = ValueNotifier({});
}

class SeatApplication {
  String id;
  String userId;
  String avatarUrl;
  String userName;
  String content;
  int timestamp;

  SeatApplication(
      {this.id = '',
      this.userId = '',
      this.avatarUrl = '',
      this.userName = '',
      this.content = '',
      this.timestamp = 0});

  SeatApplication.fromTUIRequest(TUIRequest request)
      : this(
            id: request.requestId,
            userId: request.userId,
            avatarUrl: request.avatarUrl ?? '',
            userName: request.userName ?? '',
            content: request.content,
            timestamp: request.timestamp);

  SeatApplication.fromTUIUserInfo(TUIUserInfo userInfo)
      : this(
            id: '',
            userId: userInfo.userId,
            avatarUrl: userInfo.avatarUrl,
            userName: userInfo.userName);
}

class SeatInfo {
  int index;
  String userId;
  String avatarUrl;
  String userName;
  bool isLocked;
  bool isAudioLocked;

  SeatInfo(
      {this.index = 0,
      this.userId = '',
      this.avatarUrl = '',
      this.userName = '',
      this.isLocked = false,
      this.isAudioLocked = false});

  SeatInfo.fromTUISeatInfo(TUISeatInfo seatInfo)
      : this(
            index: seatInfo.index,
            userId: seatInfo.userId,
            avatarUrl: seatInfo.avatarUrl ?? '',
            userName: seatInfo.userName ?? '',
            isLocked: seatInfo.isLocked ?? false,
            isAudioLocked: seatInfo.isAudioLocked ?? false);
}
