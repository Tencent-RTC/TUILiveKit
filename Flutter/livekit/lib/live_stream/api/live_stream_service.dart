import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';
import 'package:rtc_room_engine/api/room/tui_room_observer.dart';
import 'package:tencent_trtc_cloud/trtc_cloud.dart';
import 'package:tencent_trtc_cloud/tx_beauty_manager.dart';

class LiveStreamService {
  late final TUIRoomEngine roomEngine;
  late final TUILiveListManager liveListManager;
  late final TRTCCloud? trtcCloud;
  late final TXBeautyManager? beautyManager;

  LiveStreamService() {
    roomEngine = TUIRoomEngine.sharedInstance();
    liveListManager = roomEngine.getExtension(TUIExtensionType.liveListManager);
    _initTRTCCloud();
  }

  void addEngineObserver(TUIRoomObserver observer) {
    roomEngine.addObserver(observer);
  }

  void removeEngineObserver(TUIRoomObserver observer) {
    roomEngine.removeObserver(observer);
  }

  void addLiveListManagerObserver(TUILiveListObserver observer) {
    liveListManager.addObserver(observer);
  }

  void removeLiveListManagerObserver(TUILiveListObserver observer) {
    liveListManager.removeObserver(observer);
  }
}

extension LiveStreamServiceWithRoom on LiveStreamService {
  Future<TUIActionCallback> syncLiveInfoToService(
      TUILiveInfo liveInfo, List<TUILiveModifyFlag> modifyFlags) {
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

    return liveListManager.setLiveInfo(liveInfo.roomInfo.roomId,
        coverUrl: coverUrl,
        categoryList: categoryList,
        isPublicVisible: isPublicVisible,
        activityStatus: activityStatus,
        backgroundUrl: backgroundUrl);
  }

  Future<TUIValueCallBack<TUILiveInfo>> fetchLiveInfo(String roomId) {
    return liveListManager.getLiveInfo(roomId);
  }
}

extension LiveStreamServiceWithUser on LiveStreamService {
  TUILoginUserInfo getSelfInfo() {
    return TUIRoomEngine.getSelfInfo();
  }

  Future<TUIValueCallBack<List<TUIUserInfo>>> getUserList() async {
    final List<TUIUserInfo> allUsers = [];
    int nextSequence = 0;
    while (true) {
      final result = await roomEngine.getUserList(nextSequence);
      if (result.code != TUIError.success || result.data == null) {
        return TUIValueCallBack(code: result.code, message: result.message);
      }
      TUIUserListResult userListResult = result.data!;
      allUsers.addAll(userListResult.userInfoList);
      nextSequence = userListResult.nextSequence;
      if (nextSequence == 0) {
        break;
      }
    }
    return TUIValueCallBack(
        code: TUIError.success, message: '', data: allUsers);
  }

  Future<TUIValueCallBack<TUIUserInfo>> getUserInfo(String userId) {
    return roomEngine.getUserInfo(userId);
  }

  Future<TUIActionCallback> disableSendingMessageByAdmin(
      String userId, bool isDisable) {
    return roomEngine.disableSendingMessageByAdmin(userId, isDisable);
  }

  Future<TUIActionCallback> kickRemoteUserOutOfRoom(String userId) {
    return roomEngine.kickRemoteUserOutOfRoom(userId);
  }
}

extension LiveStreamServiceWithMedia on LiveStreamService {
  void muteAllRemoteAudio(bool isMute) {
    trtcCloud?.muteAllRemoteAudio(isMute);
  }

  void setLocalVideoView(int viewId) {
    roomEngine.setLocalVideoView(viewId);
  }

  void enableGravitySensor(bool enable) {
    roomEngine.enableGravitySensor(enable);
  }

  void setVideoResolutionMode(TUIResolutionMode resolutionMode) {
    roomEngine.setVideoResolutionMode(
        TUIVideoStreamType.cameraStream, resolutionMode);
  }

  void updateVideoQuality(TUIVideoQuality videoQuality) {
    roomEngine.updateVideoQuality(videoQuality);
  }

  void setBeautyStyle(int style) {
    beautyManager?.setBeautyStyle(style);
  }

  void updateVideoQualityEx(
      TUIVideoStreamType streamType, TUIRoomVideoEncoderParams params) {
    roomEngine.updateVideoQualityEx(streamType, params);
  }
}

extension LiveStreamServiceWithSeat on LiveStreamService {
  Future<TUIValueCallBack<List<TUISeatInfo>>> getSeatList() {
    return roomEngine.getSeatList();
  }

  Future<TUIValueCallBack<List<TUIRequest>>> getSeatApplicationList() {
    return roomEngine.getSeatApplicationList();
  }

  Future<TUIActionCallback> lockSeatByAdmin(
      int seatIndex, TUISeatLockParams lockParams) {
    return roomEngine.lockSeatByAdmin(seatIndex, lockParams);
  }
}

extension LiveStreamServiceWithCoHost on LiveStreamService {
  Future<TUIValueCallBack<TUILiveListResult>> fetchRecommendedList(
      String cursor, int count) {
    return liveListManager.fetchLiveList(cursor, count);
  }
}

extension on LiveStreamService {
  void _initTRTCCloud() async {
    trtcCloud = await TRTCCloud.sharedInstance();
    beautyManager = trtcCloud?.getBeautyManager();
  }

  bool _containsFlag({required int bitmask, required int flag}) {
    return (bitmask & flag) == flag;
  }
}
