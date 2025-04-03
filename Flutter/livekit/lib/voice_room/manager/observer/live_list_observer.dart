import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';

import '../index.dart';

class LiveListObserver extends TUILiveListObserver {
  late final Context context;

  void init(Context context) {
    this.context = context;
  }

  LiveListObserver() {
    super.onLiveInfoChanged = (liveInfo, modifyFlags) {
      context.roomManager.target?.onLiveInfoChanged(liveInfo, modifyFlags);
    };
  }
}