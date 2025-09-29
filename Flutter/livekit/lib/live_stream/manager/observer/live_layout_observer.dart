import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../module/room_manager.dart';
import '../live_stream_manager.dart';

class LiveLayoutObserver extends TUILiveLayoutObserver {
  late final Context context;

  void init(Context context) {
    this.context = context;
  }

  LiveLayoutObserver() {
    super.onLiveVideoLayoutChanged = (roomId, layoutInfo) {
      context.roomManager.target?.onLiveVideoLayoutChanged(roomId, layoutInfo);
    };
  }
}
