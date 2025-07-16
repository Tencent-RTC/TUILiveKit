import 'package:tencent_cloud_chat_sdk/enum/V2TimFriendshipListener.dart';

import 'live_info_manager.dart';

class LiveInfoIMObserver extends V2TimFriendshipListener {
  final WeakReference<LiveInfoManager> manager;

  LiveInfoIMObserver({required this.manager}) {
    // audience listen to this callback
    onMyFollowingListChanged = (userInfoList, isAdd) {
      manager.target?.onMyFollowingListChanged(userInfoList, isAdd);
    };

    // owner listen to this callback
    onMyFollowersListChanged = (userInfoList, isAdd) {
      manager.target?.onMyFollowersListChanged(userInfoList, isAdd);
    };
  }
}
