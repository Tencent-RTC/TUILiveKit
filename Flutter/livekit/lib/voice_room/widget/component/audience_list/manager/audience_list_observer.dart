import 'package:rtc_room_engine/api/room/tui_room_observer.dart';
import 'audience_list_manager.dart';

class AudienceListObserver extends TUIRoomObserver {
  final WeakReference<AudienceListManager> manager;

  AudienceListObserver({required this.manager}) {
    super.onRoomUserCountChanged = (roomId, userCount) {
      manager.target?.onRoomUserCountChanged(roomId, userCount);
    };

    super.onRemoteUserEnterRoom = (roomId, userInfo) {
      manager.target?.onRemoteUserEnterRoom(roomId, userInfo);
    };

    super.onRemoteUserLeaveRoom = (roomId, userInfo){
      manager.target?.onRemoteUserLeaveRoom(roomId, userInfo);
    };

    super.onRoomDismissed = (roomId, reason) {
      manager.target?.onRoomDismissed(roomId, reason);
    };
  }
}