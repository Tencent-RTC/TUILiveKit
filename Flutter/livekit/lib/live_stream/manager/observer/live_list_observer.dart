import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';
import 'package:tencent_live_uikit/live_stream/manager/module/index.dart';

import '../live_stream_manager.dart';

class LiveListObserver extends TUILiveListObserver {
  late final Context context;

  void init(Context context) {
    this.context = context;
  }

  LiveListObserver() {
    super.onLiveInfoChanged = (liveInfo, modifyFlagList) {
      context.roomManager.target?.onLiveInfoChanged(liveInfo, modifyFlagList);
    };
  }
}
