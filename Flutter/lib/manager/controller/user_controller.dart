import 'dart:collection';

import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/manager/index.dart';
import 'package:tencent_live_uikit/state/index.dart';

class UserController extends Controller {
  static const String tag = "UserController";
  static const int volumeCanHeardMinLimit = 25;

  UserController(super.state, super.service) {
    _initSelfUserData();
  }

  @override
  void destroy() {}

  Future<void> getAudienceList() async {
    try {
      final userListResult = await liveService.getUserList(0);

      if (TUIError.success == userListResult.code && userListResult.data != null) {
        userState.userList.value.clear();

        final userInfoSet = <UserInfo>{};
        for (final userInfo in userListResult.data!.userInfoList) {
          if (userInfo.userId == roomState.ownerInfo.userId) {
            continue;
          }
          final liveUserInfo = UserInfo.fromTUIUserInfo(userInfo);
          userInfoSet.add(liveUserInfo);
        }
        userState.addUserList(userInfoSet);
      } else {
        ErrorHandler.onError(userListResult.code);
      }
    } catch (error, stackTrace) {
      LiveKitLogger.error('getAudienceList Error: $error\n$stackTrace');
    }
  }

  void muteAllRemoteAudio(bool isMute) async {
    await liveService.muteAllRemoteAudio(isMute);
  }

  Future<void> updateOwnerUserInfo() async {
    final ownerId = roomState.ownerInfo.userId;

    if (ownerId.isEmpty) {
      return;
    }

    try {
      final result = await liveService.getUserInfo(ownerId);
      if (TUIError.success == result.code && result.data != null) {
        roomState.ownerInfo.updateState(result.data!);
      } else {
        ErrorHandler.onError(TUIError.errFailed);
      }
    } catch (error, stackTrace) {
      LiveKitLogger.error('updateOwnerUserInfo Error: $error\n$stackTrace');
    }
  }

  Future<void> followUser(String userId) async {
    final userIDList = <String>[userId];

    try {
      await liveService.followUser(userIDList: userIDList);

      userState.myFollowingUserList.value.add(UserInfo.formUserId(userId));
      LinkedHashSet<UserInfo> tempList = LinkedHashSet<UserInfo>();
      tempList.addAll(userState.myFollowingUserList.value);
      userState.myFollowingUserList.value = tempList;
      getFansCount();
      ErrorHandler.onError(TUIError.success);
    } catch (error, stackTrace) {
      ErrorHandler.onError(TUIError.errFailed);
      LiveKitLogger.error('followUser Error: $error\n$stackTrace');
    }
  }

  Future<void> unfollowUser(String userId) async {
    final userIDList = <String>[userId];

    try {
      await liveService.unfollowUser(userIDList: userIDList);

      userState.myFollowingUserList.value.removeWhere((userInfo) => userInfo.userId == userId);
      LinkedHashSet<UserInfo> tempList = LinkedHashSet<UserInfo>();
      tempList.addAll(userState.myFollowingUserList.value);
      userState.myFollowingUserList.value = tempList;
      getFansCount();
      ErrorHandler.onError(TUIError.success);
    } catch (error, stackTrace) {
      ErrorHandler.onError(TUIError.errFailed);
      LiveKitLogger.error('unfollowUser Error: $error\n$stackTrace');
    }
  }

  Future<void> checkFollowType(String userId) async {
    final userIDList = <String>[userId];

    try {
      final followTypeCheckResults = await liveService.checkFollowType(userIDList: userIDList);

      if (followTypeCheckResults.code == 0 &&
          followTypeCheckResults.data != null &&
          followTypeCheckResults.data!.isNotEmpty) {
        final result = followTypeCheckResults.data![0];
        final userInfo = UserInfo.formUserId(result.userID!);

        if (result.followType == 1 || result.followType == 3) {
          userState.myFollowingUserList.value.add(userInfo);
        } else {
          userState.myFollowingUserList.value.remove(userInfo);
        }
        LinkedHashSet<UserInfo> tempList = LinkedHashSet<UserInfo>();
        tempList.addAll(userState.myFollowingUserList.value);
        userState.myFollowingUserList.value = tempList;
      }
    } catch (error, stackTrace) {
      LiveKitLogger.error('checkFollowType Error: $error\n$stackTrace');
    }
  }

  Future<void> getFansCount() async {
    final userIDList = <String>[roomState.ownerInfo.userId];
    try {
      final followInfoList = await liveService.getUserFollowInfo(userIDList: userIDList);
      if (followInfoList.code == 0 && followInfoList.data != null && followInfoList.data!.isNotEmpty) {
        final result = followInfoList.data![0];
        roomState.ownerInfo.fansCount.value = result.followersCount!;
      }
    } catch (error, stackTrace) {
      LiveKitLogger.error('getFansCount Error: $error\n$stackTrace');
    }
  }

  void _initSelfUserData() {
    TUILoginUserInfo loginUserInfo = liveService.getSelfInfo();
    userState.selfInfo.userId = loginUserInfo.userId;
    userState.selfInfo.name.value = loginUserInfo.userName;
    userState.selfInfo.avatarUrl.value = loginUserInfo.avatarUrl;
  }

  void onUserAudioStateChanged(String userId, bool hasAudio, TUIChangeReason reason) {
    if (hasAudio) {
      userState.hasAudioStreamUserList.value.add(userId);
    } else {
      userState.hasAudioStreamUserList.value.remove(userId);
    }
    final LinkedHashSet<String> tempList = LinkedHashSet<String>();
    tempList.addAll(userState.hasAudioStreamUserList.value);
    userState.hasAudioStreamUserList.value = tempList;
    if (userId == userState.selfInfo.userId) {
      mediaState.isMicrophoneMuted.value = !hasAudio;
      if (hasAudio) {
        mediaState.isMicrophoneOpened.value = true;
      }
    }
  }

  void onUserVideoStateChanged(String userId, TUIVideoStreamType streamType, bool hasVideo, TUIChangeReason reason) {
    if (hasVideo) {
      userState.hasVideoStreamUserList.value.add(userId);
    } else {
      userState.hasVideoStreamUserList.value.remove(userId);
    }
    final LinkedHashSet<String> tempList = LinkedHashSet<String>();
    tempList.addAll(userState.hasVideoStreamUserList.value);
    userState.hasVideoStreamUserList.value = tempList;
    if (userId == userState.selfInfo.userId) {
      mediaState.isCameraOpened.value = hasVideo;
    }
  }

  void onUserVoiceVolumeChanged(Map<String, int> volumeMap) {
    volumeMap.entries.forEach((entry) {
      String userId = entry.key;
      if (entry.value > volumeCanHeardMinLimit) {
        userState.speakingUserList.value.add(userId);
      } else {
        userState.speakingUserList.value.remove(userId);
      }
    });
  }

  void onRemoteUserEnterRoom(String roomId, TUIUserInfo userInfo) {
    if (userInfo.userId == roomState.ownerInfo.userId) {
      return;
    }
    userState.addUser(userInfo);
  }

  void onRemoteUserLeaveRoom(String roomId, TUIUserInfo userInfo) {
    userState.removeUser(userInfo);
  }
}
