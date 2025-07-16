import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_info.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_user_full_info.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';

import '../../../voice_room/state/voice_define.dart';
import '../state/follow_define.dart';
import '../state/live_info_state.dart';

class LiveInfoManager {
  final state = LiveInfoState();
  late final friendshipManager =
      TencentImSDKPlugin.v2TIMManager.getFriendshipManager();

  void initRoomInfo(String roomId) async {
    state.roomId = roomId;
    state.selfUserId = TUIRoomEngine.getSelfInfo().userId;
    final result = await TUIRoomEngine.sharedInstance()
        .fetchRoomInfo(roomId: roomId, roomType: TUIRoomType.livingRoom);
    if (result.code == TUIError.success && result.data != null) {
      final TUIRoomInfo roomInfo = result.data!;
      state.ownerId.value = roomInfo.ownerId;
      state.ownerName.value = roomInfo.ownerName ?? '';
      state.ownerAvatarUrl.value = roomInfo.ownerAvatarUrl ?? '';
      _syncUserFollowingStatus(roomInfo.ownerId);
    }
  }

  void getFansNumber() async {
    final result = await friendshipManager
        .getUserFollowInfo(userIDList: [state.ownerId.value]);
    const success = 0;
    if (result.code == success &&
        result.data != null &&
        result.data!.firstOrNull != null) {
      final V2TimFollowInfo followInfo = result.data!.first;
      state.fansNumber.value = followInfo.followersCount ?? 0;
    }
  }

  void followUser(String userId) async {
    final TUIUserInfo userInfo = TUIUserInfo(
        userId: userId,
        userName: '',
        avatarUrl: '',
        userRole: TUIRole.generalUser);
    final result = await friendshipManager.followUser(userIDList: [userId]);
    const success = 0;
    if (result.code == success) {
      final Set<TUIUserInfo> followingList =
          Set.from(state.followingList.value);
      followingList.add(userInfo);
      state.followingList.value = followingList;
      getFansNumber();
    }
  }

  void unfollowUser(String userId) async {
    final result = await friendshipManager.unfollowUser(userIDList: [userId]);
    const success = 0;
    if (result.code == success) {
      final Set<TUIUserInfo> followingList =
          Set.from(state.followingList.value);
      followingList.removeWhere((following) => following.userId == userId);
      state.followingList.value = followingList;
      getFansNumber();
    }
  }

  void dispose() {
    state.dispose();
  }
}

extension LiveinfomanagerCallback on LiveInfoManager {
  void onRoomDismissed(String roomId, TUIRoomDismissedReason reason) {
    state.roomDismissedSubject.add(roomId);
  }

  void onMyFollowingListChanged(
      List<V2TimUserFullInfo> userInfoList, bool isAdd) {
    _syncUserFollowingStatus(state.ownerId.value);
  }

  void onMyFollowersListChanged(
      List<V2TimUserFullInfo> userInfoList, bool isAdd) {
    _syncUserFollowingStatus(state.ownerId.value);
  }
}

extension on LiveInfoManager {
  void _syncUserFollowingStatus(String userId) async {
    final TUIUserInfo userInfo = TUIUserInfo(
        userId: userId,
        userName: '',
        avatarUrl: '',
        userRole: TUIRole.generalUser);
    final result =
        await friendshipManager.checkFollowType(userIDList: [userId]);
    const success = 0;
    if (result.code == success &&
        result.data != null &&
        result.data!.isNotEmpty) {
      final followType = IMFollowType.fromInt(result.data![0].followType ?? 0);
      final isFollow = followType == IMFollowType.inMyFollowingList ||
          followType == IMFollowType.inBothFollowersList;

      final Set<TUIUserInfo> followingList =
          Set.from(state.followingList.value);
      if (!isFollow) {
        followingList.removeWhere((following) => following.userId == userId);
        state.followingList.value = followingList;
        return;
      }
      followingList.add(userInfo);
      state.followingList.value = followingList;
    }
  }
}
