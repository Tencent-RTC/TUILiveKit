import 'dart:async';

import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../../index.dart';
import '../../../common/index.dart';

class RoomManager {
  final RoomState state = RoomState();
  late final Context context;
  late final VoiceRoomService service;
  late final StreamController<String>? toastSubject;
  late final StreamController<void>? exitSubject;

  void init(Context context) {
    this.context = context;
    service = context.service;
    toastSubject = context.toastSubject.target;
    exitSubject = context.exitSubject.target;
  }

  Future<void> fetchRoomInfo() async {
    final result = await service.fetchRoomInfo();
    if (result.code == TUIError.success && result.data != null) {
      final TUIRoomInfo roomInfo = result.data!;
      state.roomId = roomInfo.roomId;
      state.roomName.value = roomInfo.name ?? '';
      state.ownerInfo.userId = roomInfo.ownerId;
      state.seatMode.value = roomInfo.seatMode;
      state.createTime = roomInfo.createTime;
      state.maxSeatCount.value = roomInfo.maxSeatCount;
      return;
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.rawValue, result.message) ??
        '');
  }

  Future<void> fetchLiveInfo(String roomId) async {
    final result = await service.fetchLiveInfo(roomId);
    if (result.code == TUIError.success && result.data != null) {
      final TUILiveInfo liveInfo = result.data!;
      state.coverUrl.value = liveInfo.coverUrl;
      state.liveExtraInfo.value.liveMode.value = liveInfo.isPublicVisible
          ? PrivacyStatus.publicity
          : PrivacyStatus.privacy;
      state.backgroundUrl.value = liveInfo.backgroundUrl;
      return;
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.rawValue, result.message) ??
        '');
  }

  Future<void> fetchRoomOwnerInfo(String ownerId) async {
    final result = await service.fetchRoomOwnerInfo(ownerId);
    if (result.code == TUIError.success && result.data != null) {
      final User user = result.data!;
      state.ownerInfo = user;
      return;
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.rawValue, result.message) ??
        '');
  }

  Future<void> setRoomSeatModeByAdmin(TUISeatMode seatMode) async {
    final result = await service.setRoomSeatModeByAdmin(seatMode);
    if (result.code == TUIError.success) {
      state.seatMode.value = seatMode;
      return;
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.rawValue, result.message) ??
        '');
  }

  Future<void> setLiveInfo(
      TUILiveInfo liveInfo, List<TUILiveModifyFlag> modifyFlags) async {
    final result = await _setLiveInfoInternal(liveInfo, modifyFlags);
    if (result.code == TUIError.success) {
      return _updateLiveInfo(liveInfo, modifyFlags);
    }
    toastSubject?.add(ErrorHandler.convertToErrorMessage(
            result.code.rawValue, result.message) ??
        '');
  }
}

extension VoiceRoomRoomManagerCallback on RoomManager {
  void onLiveInfoChanged(
      TUILiveInfo liveInfo, List<TUILiveModifyFlag> modifyFlags) {
    _updateLiveInfo(liveInfo, modifyFlags);
  }

  void onRoomNameChanged(String roomId, String roomName) {
    if (roomId != state.roomId) return;
    state.roomName.value = roomName;
  }

  void onMaxSeatCountChanged(int maxSeatCount) {
    state.maxSeatCount.value = maxSeatCount;
  }

  void onRoomSeatModeChanged(String roomId, TUISeatMode seatMode) {
    if (roomId != state.roomId) return;
    state.seatMode.value = seatMode;
  }

  void onRoomDismissed(String roomId) {
    exitSubject?.add(null);
  }

  void onRoomUserCountChanged(String roomId, int userCount) {
    if (roomId != state.roomId) return;
    state.userCount.value = userCount > 0 ? userCount - 1 : userCount;
    if (userCount > state.liveExtraInfo.value.maxAudienceCount) {
      state.liveExtraInfo.value.maxAudienceCount = userCount - 1;
    }
  }

  void onKickedOffLine(String message) {
    exitSubject?.add(null);
  }

  void onKickedOutOfRoom(
      String roomId, TUIKickedOutOfRoomReason reason, String message) {
    if (roomId != state.roomId) return;
    exitSubject?.add(null);
  }
}

extension VocieRoomRoomMangaerStateOperation on RoomManager {
  void onRoomIdChanged(String roomId) {
    state.roomId = roomId;
  }

  void onRoomInfoChanged(TUIRoomInfo roomInfo) {
    state.roomId = roomInfo.roomId;
    state.roomName.value = roomInfo.name ?? '';
    state.ownerInfo.userId = roomInfo.ownerId;
    state.seatMode.value = roomInfo.seatMode;
    state.createTime = roomInfo.createTime;
    state.maxSeatCount.value = roomInfo.maxSeatCount;
  }

  void onRoomOwnerInfoChanged(User user) {
    state.ownerInfo = user;
  }

  void onCoverUrlChanged(String url) {
    state.coverUrl.value = url;
  }

  void onBackgroundUrlChanged(String url) {
    state.backgroundUrl.value = url;
  }

  void onLiveModeChanged(PrivacyStatus status) {
    state.liveExtraInfo.value.liveMode.value = status;
  }
}

extension on RoomManager {
  Future<TUIActionCallback> _setLiveInfoInternal(
      TUILiveInfo liveInfo, List<TUILiveModifyFlag> modifyFlags) async {
    final bitmask = modifyFlags.fold(0, (value, flag) => value | flag.value());

    String? coverUrl = _containsFlag(
            bitmask: bitmask, flag: TUILiveModifyFlag.coverUrl.value())
        ? liveInfo.coverUrl
        : null;
    List<int>? categoryList = _containsFlag(
            bitmask: bitmask, flag: TUILiveModifyFlag.category.value())
        ? liveInfo.categoryList
        : null;
    bool? isPublicVisible =
        _containsFlag(bitmask: bitmask, flag: TUILiveModifyFlag.publish.value())
            ? liveInfo.isPublicVisible
            : null;
    int? activityStatus = _containsFlag(
            bitmask: bitmask, flag: TUILiveModifyFlag.activityStatus.value())
        ? liveInfo.activityStatus
        : null;

    String? backgroundUrl = _containsFlag(
            bitmask: bitmask, flag: TUILiveModifyFlag.backgroundUrl.value())
        ? liveInfo.backgroundUrl
        : null;

    final result = await service.setLiveInfo(liveInfo.roomInfo.roomId,
        coverUrl: coverUrl,
        backgroundUrl: backgroundUrl,
        categoryList: categoryList,
        isPublicVisible: isPublicVisible,
        activityStatus: activityStatus);
    return result;
  }

  void _updateLiveInfo(
      TUILiveInfo liveInfo, List<TUILiveModifyFlag> modifyFlags) {
    final bitmask = modifyFlags.fold(0, (value, flag) => value | flag.value());
    if (_containsFlag(
        bitmask: bitmask, flag: TUILiveModifyFlag.coverUrl.value())) {
      state.coverUrl.value = liveInfo.coverUrl;
    }
    if (_containsFlag(
        bitmask: bitmask, flag: TUILiveModifyFlag.publish.value())) {
      state.liveExtraInfo.value.liveMode.value = liveInfo.isPublicVisible
          ? PrivacyStatus.publicity
          : PrivacyStatus.privacy;
    }

    if (_containsFlag(
        bitmask: bitmask, flag: TUILiveModifyFlag.backgroundUrl.value())) {
      state.backgroundUrl.value = liveInfo.backgroundUrl;
    }
  }

  bool _containsFlag({required int bitmask, required int flag}) {
    return (bitmask & flag) == flag;
  }
}
