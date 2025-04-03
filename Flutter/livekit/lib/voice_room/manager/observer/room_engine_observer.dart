import 'package:rtc_room_engine/api/room/tui_room_observer.dart';

import '../index.dart';

class RoomEngineObserver extends TUIRoomObserver {
  late final Context context;

  void init(Context context) {
    this.context = context;
  }

  RoomEngineObserver() {
    super.onRoomNameChanged = (roomId, roomName) {
      context.roomManager.target?.onRoomNameChanged(roomId, roomName);
    };

    super.onRoomSeatModeChanged = (roomId, seatMode) {
      context.roomManager.target?.onRoomSeatModeChanged(roomId, seatMode);
    };

    super.onRoomDismissed = (roomId, reason) {
      context.roomManager.target?.onRoomDismissed(roomId);
    };

    super.onRemoteUserEnterRoom = (roomId, userInfo) {
      context.userManager.target?.onRemoteUserEnterRoom(roomId, userInfo);
    };

    super.onRemoteUserLeaveRoom = (roomId, userInfo) {
      context.userManager.target?.onRemoteUserLeaveRoom(roomId, userInfo);
    };

    super.onRoomUserCountChanged = (roomId, userCount) {
      context.roomManager.target?.onRoomUserCountChanged(roomId, userCount);
    };

    super.onKickedOffLine = (message) {
      context.roomManager.target?.onKickedOffLine(message);
    };

    super.onKickedOutOfRoom = (roomId, reason, message) {
      context.roomManager.target?.onKickedOutOfRoom(roomId, reason, message);
    };

    super.onSeatListChanged = (seatList, seatedList, leftList) {
      context.seatManager.target
          ?.onSeatListChanged(seatList, seatedList, leftList);
    };
  }
}
