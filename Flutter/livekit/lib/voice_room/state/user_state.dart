import 'package:flutter/cupertino.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_user_full_info.dart';
import 'voice_define.dart';

class UserState {
  User selfInfo = User();
  ValueNotifier<List<User>> userList = ValueNotifier([]);
  ValueNotifier<Set<User>> myFollowingUserList = ValueNotifier({});
  ValueNotifier<Set<String>> hasAudioStreamUserList = ValueNotifier({});
  ValueNotifier<Set<String>> speakingUserList = ValueNotifier({});
  ValueNotifier<User> enterUser = ValueNotifier(User());
}

class User {
  String userId;
  String name;
  String avatarUrl;
  TUIRole role;
  int fansCount;
  ValueNotifier<LinkStatus> linkStatus = ValueNotifier(LinkStatus.none);

  User(
      {this.userId = '',
      this.name = '',
      this.avatarUrl = '',
      this.role = TUIRole.generalUser,
      this.fansCount = 0,
      LinkStatus linkStatus = LinkStatus.none})
      : linkStatus = ValueNotifier(linkStatus);

  User.fromTUIUserInfo(TUIUserInfo userInfo)
      : this(
            userId: userInfo.userId,
            name: userInfo.userName,
            avatarUrl: userInfo.avatarUrl,
            role: userInfo.userRole);

  User.fromTUILoginUserInfo(TUILoginUserInfo loginUserInfo)
      : this(
            userId: loginUserInfo.userId,
            name: loginUserInfo.userName ?? '',
            avatarUrl: loginUserInfo.avatarUrl ?? '');

  User.fromV2TIMUserFullInfo(V2TimUserFullInfo imUserInfo)
      : this(
            userId: imUserInfo.userID ?? '',
            name: imUserInfo.nickName ?? '',
            avatarUrl: imUserInfo.faceUrl ?? '');
}
