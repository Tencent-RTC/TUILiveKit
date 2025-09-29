import 'dart:convert';
import 'dart:ui';

import 'package:flutter/services.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

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

  void onStartLive(bool isJoinSelf, TUILiveInfo liveInfo) {
    roomState.roomId = liveInfo.roomId;
    roomState.liveInfo = liveInfo;
    roomState.createTime = liveInfo.createTime;
    roomState.roomName = liveInfo.name;
    roomState.liveStatus.value = LiveStatus.pushing;
  }

  void onJoinLive(TUILiveInfo liveInfo) async {
    roomState.roomId = liveInfo.roomId;
    roomState.liveInfo = liveInfo;
    roomState.createTime = liveInfo.createTime;
    roomState.roomName = liveInfo.name;
    roomState.liveStatus.value = LiveStatus.playing;
    _updateLiveInfo(liveInfo);
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

  void onKickedOutOfRoom(String roomId, TUIKickedOutOfRoomReason reason, String message) {
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

  void onLiveInfoChanged(TUILiveInfo liveInfo, List<TUILiveModifyFlag> modifyFlags) {
    _updateLiveInfo(liveInfo, updateRoomInfo: false, modifyFlags: modifyFlags);
  }

  void onLiveVideoLayoutChanged(String roomId, String layoutInfo) {
    if (roomId != roomState.roomId) return;
    var size = _parseCanvasSize(layoutInfo);
    if (size == null) return;
    var isLandscape = size.width >= size.height;
    if (!isLandscape && roomState.roomVideoStreamIsLandscape.value) {
      SystemChrome.setPreferredOrientations([
        DeviceOrientation.portraitUp,
      ]);
    }
    roomState.roomVideoStreamIsLandscape.value = isLandscape;
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
      roomState.roomId = liveInfo.roomId;
      roomState.createTime = liveInfo.createTime;
      roomState.roomName = liveInfo.name ?? '';
    }

    if (modifyFlags.contains(TUILiveModifyFlag.coverUrl)) {
      roomState.coverUrl.value = liveInfo.coverUrl;
    }

    if (modifyFlags.contains(TUILiveModifyFlag.publish)) {
      roomState.liveExtraInfo.liveMode =
          liveInfo.isPublicVisible ? LiveStreamPrivacyStatus.public : LiveStreamPrivacyStatus.privacy;
    }

    if (modifyFlags.contains(TUILiveModifyFlag.activityStatus)) {
      roomState.liveExtraInfo.activeStatus = liveInfo.activityStatus;
    }
  }

  Size? _parseCanvasSize(String jsonStr) {
    try {
      final data = json.decode(jsonStr);
      if (data is Map && data.containsKey('canvas')) {
        final canvas = data['canvas'];
        if (canvas is Map && canvas.containsKey('w') && canvas.containsKey('h')) {
          final w = canvas['w'];
          final h = canvas['h'];
          if (w is int && h is int) {
            return Size(w.toDouble(), h.toDouble());
          }
        }
      }
    } catch (e) {
      return null;
    }
    return null;
  }
}
