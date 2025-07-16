import 'package:rtc_room_engine/api/room/tui_room_observer.dart';
import 'package:tencent_live_uikit/live_stream/manager/module/index.dart';

import '../live_stream_manager.dart';

class RoomEngineObserver extends TUIRoomObserver {
  late final Context context;

  void init(Context context) {
    this.context = context;
  }

  RoomEngineObserver() {
    super.onRoomDismissed = (roomId, reason) {
      context.roomManager.target?.onLiveEnd(roomId);
    };
    super.onRoomUserCountChanged = (roomId, userCount) {
      context.roomManager.target?.onRoomUserCountChanged(roomId, userCount);
    };
    super.onUserVoiceVolumeChanged = (volumeMap) {
      context.userManager.target?.onUserVoiceVolumeChanged(volumeMap);
    };
    super.onRemoteUserEnterRoom = (roomId, userInfo) {
      context.userManager.target?.onRemoteUserEnterRoom(roomId, userInfo);
    };
    super.onRemoteUserLeaveRoom = (roomId, userInfo) {
      context.userManager.target?.onRemoteUserLeaveRoom(roomId, userInfo);
    };
    super.onKickedOffLine = (message) {
      // TODO: krab need to confirm whether needs to implement here with natives
    };
    super.onKickedOutOfRoom = (roomId, reason, message) {
      context.roomManager.target?.onKickedOutOfRoom(roomId, reason, message);
    };
    super.onUserInfoChanged = (userInfo, modifyFlagList) {
      context.userManager.target?.onUserInfoChanged(userInfo, modifyFlagList);
    };
    super.onSendMessageForUserDisableChanged = (roomId, userId, isDisable) {
      context.userManager.target
          ?.onSendMessageForUserDisableChanged(roomId, userId, isDisable);
    };
  }
}
