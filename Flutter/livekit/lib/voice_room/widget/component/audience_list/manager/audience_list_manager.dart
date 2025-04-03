import 'dart:math';

import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';

import '../state/audience_list_state.dart';

class AudienceListManager {
  final AudienceListState state = AudienceListState();
  DateTime? _lastFetchDate;

  void initRoomInfo(String roomId) async {
    state.roomId = roomId;
    final result = await TUIRoomEngine.sharedInstance()
        .fetchRoomInfo(roomId: roomId, roomType: TUIRoomType.livingRoom);
    if (result.code == TUIError.success && result.data != null) {
      final TUIRoomInfo roomInfo = result.data!;
      state.ownerId = roomInfo.ownerId;
      getUserList();
    }
  }

  void getUserList() async {
    final result = await TUIRoomEngine.sharedInstance().getUserList(0);
    if (result.code == TUIError.success &&
        result.data != null &&
        result.data!.userInfoList.isNotEmpty) {
      final List<TUIUserInfo> userInfoList = result.data!.userInfoList
          .sublist(0, min(maxShowUserCount, result.data!.userInfoList.length));

      userInfoList.removeWhere((userInfo) => userInfo.userId == state.ownerId);

      if (userInfoList.length > maxShowUserCount) {
        userInfoList.removeLast();
      }

      state.audienceList.value = userInfoList;
    }
  }

  void dispose() {
    state.dispose();
  }
}

extension AudienceListManagerCallback on AudienceListManager {
  void onRoomUserCountChanged(String roomId, int userCount) {
    if (userCount > maxShowUserCount) {
      state.audienceCount.value = userCount - 1;
    }
  }

  void onRemoteUserEnterRoom(String roomId, TUIUserInfo userInfo) {
    if (state.ownerId == userInfo.userId) return;
    if (state.audienceList.value
        .any((audience) => audience.userId == userInfo.userId)) {
      return;
    }
    if (state.audienceList.value.length < maxShowUserCount) {
      final List<TUIUserInfo> userList = List.from(state.audienceList.value);
      userList.add(userInfo);
      state.audienceList.value = userList;
      return;
    }
    final current = DateTime.now();
    if (_lastFetchDate != null &&
        current.difference(_lastFetchDate!).inSeconds < 10) {
      return;
    }
    _lastFetchDate = current;
    getUserList();
  }

  void onRemoteUserLeaveRoom(String roomId, TUIUserInfo userInfo) {
    final List<TUIUserInfo> userList = List.from(state.audienceList.value);
    userList.removeWhere((user) => user.userId == userInfo.userId);
    state.audienceList.value = userList;
  }

  void onRoomDismissed(String roomId, TUIRoomDismissedReason reason) {
    state.roomDismissedSubject.add(roomId);
  }
}
