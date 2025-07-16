import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/index.dart';

import '../../api/live_stream_service.dart';
import '../../state/user_state.dart';
import '../live_stream_manager.dart';

class UserManager {
  LSUserState userState = LSUserState();

  late final Context context;
  late final LiveStreamService service;

  void init(Context context) {
    this.context = context;
    service = context.service;
  }

  void dispose() {}

  final int _volumeCanHeartMinLimit = 25;

  void onLeaveLive() {
    userState = LSUserState();
  }

  Future<TUIValueCallBack<TUIUserInfo>> getUserInfo(String userId) {
    return service.getUserInfo(userId);
  }

  Future<TUIActionCallback> onDisableSendingMessageBtnClicked(
      String userId, bool isDisable) {
    return service.disableSendingMessageByAdmin(userId, isDisable);
  }

  Future<TUIActionCallback> onKickedOutBtnClicked(String userId) {
    return service.kickRemoteUserOutOfRoom(userId);
  }
}

extension UserManagerCallback on UserManager {
  void onUserVoiceVolumeChanged(Map<String, int> volumeMap) {
    for (final entry in volumeMap.entries) {
      entry.value > _volumeCanHeartMinLimit
          ? userState.speakingUserList.add(entry.key)
          : userState.speakingUserList.remove(entry.key);
    }
  }

  void onRemoteUserEnterRoom(String roomId, TUIUserInfo userInfo) {
    if (roomId != context.roomManager.target?.roomState.roomId) {
      return;
    }

    if (userInfo.userId == context.coreRoomState.ownerInfo.userId) {
      return;
    }

    userState.userList.value.add(userInfo);
    userState.enterUser.value = userInfo;
  }

  void onRemoteUserLeaveRoom(String roomId, TUIUserInfo userInfo) {
    if (roomId != context.roomManager.target?.roomState.roomId) {
      return;
    }

    userState.userList.value
        .removeWhere((user) => user.userId == userInfo.userId);
  }

  void onUserInfoChanged(
      TUIUserInfo userInfo, List<TUIUserInfoModifyFlag> modifyFlags) {
    for (var user in userState.userList.value) {
      if (user.userId == userInfo.userId &&
          modifyFlags.contains(TUIUserInfoModifyFlag.userRole)) {
        user.userRole = user.userRole;
      }
    }

    userState.userList.value = userState.userList.value.toSet();
  }

  void onSendMessageForUserDisableChanged(
      String roomId, String userId, bool isDisable) {
    if (roomId == context.coreRoomState.roomId &&
        userId == context.coreUserState.selfInfo.userId) {
      final toast = isDisable
          ? LiveKitLocalizations.of(Global.appContext())!
              .common_client_error_send_message_disabled_for_current
          : LiveKitLocalizations.of(Global.appContext())!
              .common_send_message_enable;
      context.toastSubject.target?.add(toast);
    }
  }
}
