import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

class LSUserState {
  final ValueNotifier<Set<TUIUserInfo>> userList = ValueNotifier({});
  Set<String> speakingUserList = {};
  Set<TUIUserInfo> myFollowingUserList = {};
  ValueNotifier<TUIUserInfo> enterUser = ValueNotifier(TUIUserInfo(
      userId: '', userName: '', userRole: TUIRole.generalUser, avatarUrl: ''));
}
