import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/extension/tui_live_connection_manager.dart';
import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';
import 'package:tencent_live_uikit/common/index.dart';

import '../../api/live_stream_service.dart';
import '../../state/co_host_state.dart';
import '../live_stream_manager.dart';

class CoHostManager {
  LSCoHostState coHostState = LSCoHostState();

  late final Context context;
  late final LiveStreamService service;

  void init(Context context) {
    this.context = context;
    service = context.service;
    coHostState.currentRoomId = context.roomManager.target?.roomState.roomId ?? '';
  }

  final int _listCount = 20;

  void dispose() {}

  bool get isCoHostConnecting => coHostState.connectedUsers.value.isNotEmpty;

  Future<void> fetchRecommendedList({String cursor = ''}) async {
    final cursor = coHostState.recommendListCursor.value;
    final result = await service.fetchRecommendedList(cursor, _listCount);
    if (result.code != TUIError.success || result.data == null) {
      LiveKitLogger.error('fetchRecommendedList failed. code:${result.code}, message:${result.message}');
    }
    final recommendListResult = result.data!;
    if (cursor.isEmpty) {
      coHostState.recommendedUsers.value.clear();
    }

    final List<TUIConnectionUser> recommendUsers =
        recommendListResult.liveInfoList
            .map((liveInfo) {
              final isConnected = coHostState.connectedUsers.value
                  .any((user) => user.roomId == liveInfo.roomId);
              if (!isConnected) {
                final user = _convertLiveInfo2ConnectionUser(liveInfo);
                if (context.coreCoHostState.sentConnectionRequestList.value
                    .any((invitee) => invitee.userId == user.userId)) {
                  user.connectionStatus = TUIConnectionStatus.inviting;
                }
                return user;
              } else {
                return TUIConnectionUser();
              }
            })
            .where((user) => user.roomId.isNotEmpty)
            .toList();

    final List<TUIConnectionUser> newRecommendedUsers = coHostState.recommendedUsers.value.toList();
    newRecommendedUsers.addAll(recommendUsers);

    coHostState.recommendedUsers.value = newRecommendedUsers;
    coHostState.recommendListCursor.value = recommendListResult.cursor;
  }

  void setLayoutTemplateId(int id) {
    coHostState.templateId = id;
  }

  void setCoHostLayoutTemplateId() {
    service.setCoHostLayoutTemplateId(coHostState.templateId);
  }
}

extension CoHostManagerCallback on CoHostManager {
  void onCrossRoomConnectionTerminated() {
    coHostState.connectedUsers.value = [];
  }

  void onRequestConnection(TUIConnectionUser user) {
    final newRecommendUsers = coHostState.recommendedUsers.value.toList();
    for (var recommendedUser in newRecommendUsers) {
      if (recommendedUser.roomId == user.roomId) {
        recommendedUser.connectionStatus = TUIConnectionStatus.inviting;
      }
    }
    coHostState.recommendedUsers.value = newRecommendUsers;
  }

  void onRequestConnectionFailed(String roomId) {
    final newRecommendUsers = coHostState.recommendedUsers.value.toList();
    for (var recommendedUser in newRecommendUsers) {
      if (recommendedUser.roomId == roomId) {
        recommendedUser.connectionStatus = TUIConnectionStatus.none;
      }
    }
    coHostState.recommendedUsers.value = newRecommendUsers;
  }

  void onConnectionUserListChanged(List<TUIConnectionUser> connectedUserList) {
    for (var user in connectedUserList) {
      user.connectionStatus = TUIConnectionStatus.connected;
    }
    coHostState.connectedUsers.value = connectedUserList.toList();

    final newRecommendUsers = coHostState.recommendedUsers.value.toList();
    newRecommendUsers.removeWhere((recommendUser) => connectedUserList
        .any((connectedUser) => connectedUser.roomId == recommendUser.roomId));
    coHostState.recommendedUsers.value = newRecommendUsers;
  }

  void onConnectionRequestReceived(TUIConnectionUser inviter) {

  }

  void onConnectionRequestAccept(TUIConnectionUser invitee) {
    final newRecommendUsers = coHostState.recommendedUsers.value.toList();
    for (var recommendedUser in newRecommendUsers) {
      if (recommendedUser.roomId == invitee.roomId) {
        recommendedUser.connectionStatus = TUIConnectionStatus.connected;
      }
    }
    coHostState.recommendedUsers.value = newRecommendUsers;
  }

  void onConnectionRequestTimeout(TUIConnectionUser inviter, TUIConnectionUser invitee) {
    final newRecommendUsers = coHostState.recommendedUsers.value.toList();
    for (var recommendedUser in newRecommendUsers) {
      if (inviter.roomId == coHostState.currentRoomId &&
          recommendedUser.roomId == invitee.roomId) {
        recommendedUser.connectionStatus = TUIConnectionStatus.none;
      }
    }
    coHostState.recommendedUsers.value = newRecommendUsers;
  }

  void onConnectionRequestReject(TUIConnectionUser invitee) {
    final newRecommendUsers = coHostState.recommendedUsers.value.toList();
    for (var recommendedUser in newRecommendUsers) {
      if (recommendedUser.roomId == invitee.roomId) {
        recommendedUser.connectionStatus = TUIConnectionStatus.none;
      }
    }
    coHostState.recommendedUsers.value = newRecommendUsers;

    final toast = LiveKitLocalizations.of(Global.appContext())!.common_connect_request_rejected;
    context.toastSubject.target?.add(toast);
  }
}

extension on CoHostManager {
  TUIConnectionUser _convertLiveInfo2ConnectionUser(TUILiveInfo liveInfo) {
    final connectionUser = TUIConnectionUser();
    connectionUser.roomId = liveInfo.roomId;
    connectionUser.userId = liveInfo.ownerId;
    connectionUser.userName = liveInfo.ownerName;
    connectionUser.avatarUrl = liveInfo.ownerAvatarUrl;
    connectionUser.joinConnectionTime = 0;
    return connectionUser;
  }
}
