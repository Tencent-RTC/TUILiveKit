import 'dart:async';

import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_user_full_info.dart';

import '../../index.dart';
import '../../../common/index.dart';

class UserManager {
  final UserState state = UserState();
  late final Context context;
  late final VoiceRoomService service;
  late final StreamController<String>? toastSubject;

  void init(Context context) {
    this.context = context;
    service = context.service;
    toastSubject = context.toastSubject.target;

    fetchSelfInfo();
  }

  Future<void> fetchUserList() async {
    final result = await service.fetchUserList();
    if (result.code == TUIError.success && result.data != null) {
      final List<User> users = result.data!;
      state.userList.value = users;
      return;
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.value(), result.message) ??
        '');
  }

  void fetchSelfInfo() {
    final selfInfo = service.getSelfInfo();
    state.selfInfo = User.fromTUILoginUserInfo(selfInfo);
  }

  Future<void> followUser({required User user, required bool isFollow}) async {
    final result = isFollow
        ? await service.followUser(user.userId)
        : await service.unfollowUser(user.userId);
    if (result.code == TUIError.success) {
      return _updateFollowUserList(user, isFollow);
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.value(), result.message) ??
        '');
  }

  Future<void> checkFollowType({required String userId}) async {
    final result = await service.checkFollowType(userId);
    if (result.code == TUIError.success && result.data != null) {
      final user = User();
      user.userId = userId;
      final IMFollowType type = result.data!;
      final isFollow = type == IMFollowType.inMyFollowingList ||
          type == IMFollowType.inBothFollowersList;
      return _updateFollowUserList(user, isFollow);
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.value(), result.message) ??
        '');
  }
}

extension VoiceRoomUserManagerCallback on UserManager {
  void onRemoteUserEnterRoom(String roomId, TUIUserInfo userInfo) {
    final List<User> userList = List.from(state.userList.value);
    if (!userList.any((user) => user.userId == userInfo.userId)) {
      userList.add(User.fromTUIUserInfo(userInfo));
      state.userList.value = userList;
    }

    state.enterUser.value = User.fromTUIUserInfo(userInfo);
  }

  void onRemoteUserLeaveRoom(String roomId, TUIUserInfo userInfo) {
    final List<User> userList = List.from(state.userList.value);
    userList.removeWhere((user) => user.userId == userInfo.userId);
    state.userList.value = userList;
  }

  void onMyFollowingListChanged(
      List<V2TimUserFullInfo> userInfoList, bool isAdd) {
    final Set<User> myFollowingUserList =
        Set.from(state.myFollowingUserList.value);
    if (isAdd) {
      final Set<User> newFollowingUsers = userInfoList
          .map((imUserInfo) => User.fromV2TIMUserFullInfo(imUserInfo))
          .toSet();
      myFollowingUserList.addAll(newFollowingUsers);
    } else {
      final Set<String> userIdsToRemove =
          userInfoList.map((imUserInfo) => imUserInfo.userID ?? '').toSet();
      myFollowingUserList.removeWhere((followUser) {
        return userIdsToRemove.any((userId) => userId == followUser.userId);
      });
    }
    state.myFollowingUserList.value = myFollowingUserList;
  }
}

extension UserManagerStateOperation on UserManager {
  void onLinkStatusChanged(LinkStatus status) {
    state.selfInfo.linkStatus.value = status;
  }
}

extension on UserManager {
  void _updateFollowUserList(User user, bool isFollow) {
    final Set<User> myFollowingUserList =
        Set.from(state.myFollowingUserList.value);
    if (isFollow) {
      if (!myFollowingUserList
          .any((followUser) => followUser.userId == user.userId)) {
        myFollowingUserList.add(user);
      } else {
        myFollowingUserList
            .removeWhere((followUser) => followUser.userId == user.userId);
      }
      state.myFollowingUserList.value = myFollowingUserList;
    }
  }
}
