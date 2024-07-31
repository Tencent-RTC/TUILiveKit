import 'package:flutter/cupertino.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_info.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_user_full_info.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';
import 'package:tencent_live_uikit_example/src/store/app_store.dart';

class AppManager {
  static const String tag = 'IMManager';

  static Future<void> getUserInfo(String userId) async {
    if (userId.isEmpty) {
      return;
    }
    var result = await TencentImSDKPlugin.v2TIMManager.getUsersInfo(userIDList: [userId]);
    AppStore.userName.value = result.data?[0].nickName ?? "";
    AppStore.userAvatar = result.data?[0].faceUrl ?? "";
    return;
  }

  static Future<TUIResult> setSelfInfo(String avatar, String nickname) async {
    if (nickname.isEmpty) {
      return TUIResult(code: TUIError.errFailed.value(), message: "The nickname cannot be empty");
    }

    V2TimUserFullInfo userFullInfo = V2TimUserFullInfo();
    userFullInfo.nickName = nickname;
    userFullInfo.faceUrl = avatar;
    var result = await TencentImSDKPlugin.v2TIMManager.setSelfInfo(userFullInfo: userFullInfo);
    if (result.code == 0) {
      AppStore.userName.value = nickname;
      AppStore.userAvatar = avatar;
    }
    return TUIResult(code: result.code, message: result.desc);
  }

  static void getUserFollowInfo(String userId) async {
    List<String> userIDList = [userId];

    var result = await TencentImSDKPlugin.v2TIMManager.getFriendshipManager().getUserFollowInfo(userIDList: userIDList);
    if (result.data == null) {
      debugPrint("getUserFollowInfo result(code: $result.code), msg:${result.desc}");
      return;
    }
    List<V2TimFollowInfo>? followInfo = result.data;
    if (followInfo != null && followInfo.isNotEmpty) {
      V2TimFollowInfo result = followInfo[0];
      AppStore.followCount.value = (result.followingCount == null ? 0 : result.followingCount!);
      AppStore.fansCount.value = (result.followersCount == null ? 0 : result.followersCount!);
    }
  }
}

class TUIResult {
  int code;
  String? message;

  TUIResult({
    required this.code,
    required this.message,
  });
}
