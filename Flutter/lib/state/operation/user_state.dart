import 'dart:collection';
import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

class UserState {
  final UserInfo selfInfo = UserInfo();
  final ValueNotifier<LinkedHashSet<UserInfo>> userList = ValueNotifier(LinkedHashSet<UserInfo>());
  final ValueNotifier<LinkedHashSet<String>> hasAudioStreamUserList = ValueNotifier(LinkedHashSet<String>());
  final ValueNotifier<LinkedHashSet<String>> hasVideoStreamUserList = ValueNotifier(LinkedHashSet<String>());
  final ValueNotifier<LinkedHashSet<String>> hasScreenStreamUserList = ValueNotifier(LinkedHashSet<String>());
  final ValueNotifier<LinkedHashSet<String>> speakingUserList = ValueNotifier(LinkedHashSet<String>());
  final ValueNotifier<LinkedHashSet<UserInfo>> myFollowingUserList = ValueNotifier(LinkedHashSet<UserInfo>());

  void reset() {
    userList.value.clear();
    hasAudioStreamUserList.value.clear();
    hasVideoStreamUserList.value.clear();
    hasScreenStreamUserList.value.clear();
    speakingUserList.value.clear();
    myFollowingUserList.value.clear();
  }

  void addUserList(Set<UserInfo> list) {
    if (list.isEmpty) {
      return;
    }
    userList.value.addAll(list);
    final userListValue = LinkedHashSet<UserInfo>();
    userListValue.addAll(list);
    userList.value = userListValue;
  }

  void addUser(TUIUserInfo userInfo) {
    if (userInfo.userId.isEmpty) {
      return;
    }
    userList.value.add(UserInfo.fromTUIUserInfo(userInfo));
    final userListValue = LinkedHashSet<UserInfo>();
    userListValue.addAll(userList.value);
    userList.value = userListValue;
  }

  void removeUser(TUIUserInfo userInfo) {
    if (userInfo.userId.isEmpty) {
      return;
    }
    userList.value.removeWhere((element) => element.userId == userInfo.userId);
    final userListValue = LinkedHashSet<UserInfo>();
    userListValue.addAll(userList.value);
    userList.value = userListValue;
  }
}

class UserInfo {
  String userId = '';
  ValueNotifier<String?> name = ValueNotifier('');
  ValueNotifier<String?> avatarUrl = ValueNotifier('');
  ValueNotifier<TUIRole> role = ValueNotifier(TUIRole.generalUser);
  ValueNotifier<int> fansCount = ValueNotifier(0);

  UserInfo();

  UserInfo.formUserId(this.userId);

  UserInfo.fromTUIUserInfo(TUIUserInfo userInfo) {
    updateState(userInfo);
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is UserInfo && other.userId == userId;
  }

  @override
  int get hashCode => userId.hashCode;

  void updateState(TUIUserInfo userInfo) {
    userId = userInfo.userId;
    name.value = userInfo.userName;
    avatarUrl.value = userInfo.avatarUrl;
    role.value = userInfo.userRole;
  }
}
