import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:tencent_live_uikit/live_stream/manager/module/co_host_manager.dart';

import '../live_stream_manager.dart';

class LiveStreamObserver extends ConnectionObserver {
  late final Context context;

  void init(Context context) {
    this.context = context;
  }

  LiveStreamObserver() {
    super.onConnectedUsersUpdated = (userList, joinList, leaveList) {};
    super.onUserConnectionRequest = (inviterUser) {};
    super.onUserConnectionCancelled = (inviterUser) {};
    super.onUserConnectionAccepted = (userInfo) {};
    super.onUserConnectionRejected = (userInfo) {
      context.coGuestManager.target?.onUserConnectionRejected(userInfo.userId);
    };
    super.onUserConnectionTimeout = (userInfo) {
      context.coGuestManager.target?.onUserConnectionTimeout(userInfo.userId);
    };
    super.onUserConnectionTerminated = () {
      context.coGuestManager.target?.onKickedOffSeat();
    };
    super.onUserConnectionExited = (userInfo) {};

    super.onConnectedRoomsUpdated = (hostUserList) {
      context.coHostManager.target?.onConnectionUserListChanged(hostUserList);
    };
    super.onCrossRoomConnectionRequest = (inviter) {
      context.coHostManager.target?.onConnectionRequestReceived(inviter);
    };
    super.onCrossRoomConnectionCancelled = (hostUser) {};
    super.onCrossRoomConnectionAccepted = (invitee) {
      context.coHostManager.target?.onConnectionRequestAccept(invitee);
    };
    super.onCrossRoomConnectionRejected = (invitee) {
      context.coHostManager.target?.onConnectionRequestReject(invitee);
    };
    super.onCrossRoomConnectionTimeout = (inviter, invitee) {
      context.coHostManager.target
          ?.onConnectionRequestTimeout(inviter, invitee);
    };
    super.onCrossRoomConnectionExited = (hostUser) {};
    super.onRoomDismissed = (roomId) {};
  }
}
