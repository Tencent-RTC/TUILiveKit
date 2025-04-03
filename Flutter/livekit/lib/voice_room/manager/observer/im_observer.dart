import 'package:tencent_cloud_chat_sdk/enum/V2TimFriendshipListener.dart';

import '../index.dart';

class IMObserver extends V2TimFriendshipListener {
  late final Context context;

  void init(Context context) {
    this.context = context;
  }

  IMObserver() {
    super.onMyFollowingListChanged = (userInfoList, isAdd) {
      context.userManager.target?.onMyFollowingListChanged(userInfoList, isAdd);
    };
  }
}