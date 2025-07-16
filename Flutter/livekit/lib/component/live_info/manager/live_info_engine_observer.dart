import 'package:rtc_room_engine/api/room/tui_room_observer.dart';

import 'live_info_manager.dart';

class LiveInfoEngineObserver extends TUIRoomObserver {
  final WeakReference<LiveInfoManager> manager;

  LiveInfoEngineObserver({required this.manager}) {
    super.onRoomDismissed = (roomId, reason) {
      manager.target?.onRoomDismissed(roomId, reason);
    };
  }
}
