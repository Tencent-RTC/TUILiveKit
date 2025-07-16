import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/index.dart';

import '../../live_define.dart';
import '../../api/live_stream_service.dart';
import '../../state/room_state.dart';
import '../live_stream_manager.dart';

class RoomManager {
  LSRoomState roomState = LSRoomState();

  late final Context context;
  late final LiveStreamService service;

  void init(Context context) {
    this.context = context;
    service = context.service;
  }

  void dispose() {}

  void prepareLiveInfoBeforeEnterRoom(TUILiveInfo liveInfo) {
    _updateLiveInfo(liveInfo);
  }

  void prepareRoomIdBeforeEnterRoom(String roomId) {
    roomState.roomId = roomId;
  }

  void onStartPreview() {
    roomState.liveStatus.value = LiveStatus.previewing;
  }

  void onStartLive(bool isJoinSelf, TUIRoomInfo roomInfo) {
    roomState.roomId = roomInfo.roomId;
    roomState.createTime = roomInfo.createTime;
    roomState.roomName = roomInfo.name ?? '';
    roomState.liveStatus.value = LiveStatus.pushing;

    if (!isJoinSelf) {
      _syncLiveInfoToService();
    }
  }

  void onJoinLive(TUIRoomInfo roomInfo) async {
    roomState.roomId = roomInfo.roomId;
    roomState.createTime = roomInfo.createTime;
    roomState.roomName = roomInfo.name ?? '';
    roomState.liveStatus.value = LiveStatus.playing;

    final result = await service.fetchLiveInfo(roomInfo.roomId);
    if (result.code != TUIError.success || result.data == null) {
      LiveKitLogger.error(ErrorHandler.convertToErrorMessage(
              result.code.rawValue, result.message) ??
          '');
      return;
    }
    _updateLiveInfo(result.data!);
  }

  void onStopLive() {
    roomState.liveStatus.value = LiveStatus.finished;
  }

  void onLeaveLive() {
    roomState = LSRoomState();
  }

  String getDefaultRoomName() {
    final selfInfo = context.coreUserState.selfInfo;
    return selfInfo.userName.isEmpty ? selfInfo.userId : selfInfo.userName;
  }

  Future<TUIValueCallBack<TUILiveInfo>> fetchLiveInfo(String roomId) async {
    final result = await service.fetchLiveInfo(roomId);
    if (result.code != TUIError.success || result.data == null) {
      return TUIValueCallBack(code: result.code, message: result.message);
    }
    _updateLiveInfo(result.data!);
    return result;
  }

  void onSetRoomName(String name) {
    roomState.roomName = name;
  }

  void onSetRoomPrivacy(LiveStreamPrivacyStatus mode) {
    roomState.liveExtraInfo.liveMode = mode;
  }

  void onSetRoomCoverUrl(String url) {
    roomState.coverUrl.value = url;
  }

  void onReceiveGift(int price, String senderUserId) {
    roomState.liveExtraInfo.giftIncome += price;
    roomState.liveExtraInfo.giftPeopleSet.add(senderUserId);
  }
}

extension RoomManagerCallBack on RoomManager {
  void onLiveEnd(String roomId) {
    if (roomId != roomState.roomId) {
      return;
    }
    roomState.liveStatus.value = LiveStatus.finished;
  }

  void onKickedOutOfRoom(
      String roomId, TUIKickedOutOfRoomReason reason, String message) {
    if (roomId != roomState.roomId) {
      return;
    }
    context.kickedOutSubject.target?.add(null);
  }

  void onRoomUserCountChanged(String roomId, int userCount) {
    if (roomId != roomState.roomId) {
      return;
    }
    if (userCount > 0) {
      roomState.userCount = userCount - 1;
      if (userCount > roomState.liveExtraInfo.maxAudienceCount) {
        roomState.liveExtraInfo.maxAudienceCount = userCount - 1;
      }
    }
  }

  void onLiveInfoChanged(
      TUILiveInfo liveInfo, List<TUILiveModifyFlag> modifyFlags) {
    _updateLiveInfo(liveInfo, updateRoomInfo: false, modifyFlags: modifyFlags);
  }
}

extension on RoomManager {
  void _updateLiveInfo(TUILiveInfo liveInfo,
      {bool updateRoomInfo = true,
      List<TUILiveModifyFlag> modifyFlags = const [
        TUILiveModifyFlag.activityStatus,
        TUILiveModifyFlag.category,
        TUILiveModifyFlag.publish,
        TUILiveModifyFlag.coverUrl
      ]}) {
    if (updateRoomInfo) {
      roomState.roomId = liveInfo.roomInfo.roomId;
      roomState.createTime = liveInfo.roomInfo.createTime;
      roomState.roomName = liveInfo.roomInfo.name ?? '';
    }

    if (modifyFlags.contains(TUILiveModifyFlag.coverUrl)) {
      roomState.coverUrl.value = liveInfo.coverUrl;
    }

    if (modifyFlags.contains(TUILiveModifyFlag.publish)) {
      roomState.liveExtraInfo.liveMode = liveInfo.isPublicVisible
          ? LiveStreamPrivacyStatus.public
          : LiveStreamPrivacyStatus.privacy;
    }

    if (modifyFlags.contains(TUILiveModifyFlag.activityStatus)) {
      roomState.liveExtraInfo.activeStatus = liveInfo.activityStatus;
    }
  }

  void _syncLiveInfoToService() async {
    final liveInfo = TUILiveInfo();
    liveInfo.roomInfo.roomId = roomState.roomId;
    liveInfo.coverUrl = roomState.coverUrl.value;
    liveInfo.isPublicVisible =
        roomState.liveExtraInfo.liveMode == LiveStreamPrivacyStatus.public;

    final result = await service.syncLiveInfoToService(
        liveInfo, [TUILiveModifyFlag.coverUrl, TUILiveModifyFlag.publish]);
    if (result.code != TUIError.success) {
      context.toastSubject.target?.add(ErrorHandler.convertToErrorMessage(
              result.code.rawValue, result.message) ??
          '');
      LiveKitLogger.error(
          'syncLiveInfoToService failed. code:${result.code}, message:${result.message}');
    }
  }
}
