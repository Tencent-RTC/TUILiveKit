import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/enum/V2TimFriendshipListener.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';

import '../state/index.dart';

class VoiceRoomService {
  final TUIRoomEngine roomEngine = TUIRoomEngine.sharedInstance();
  late final TUILiveListManager liveListManager =
      roomEngine.getExtension(TUIExtensionType.liveListManager);
  late final friendshipManager =
      TencentImSDKPlugin.v2TIMManager.getFriendshipManager();

  void addEngineObserver(TUIRoomObserver engineObserver) {
    roomEngine.addObserver(engineObserver);
  }

  void removeEngineObserver(TUIRoomObserver engineObserver) {
    roomEngine.removeObserver(engineObserver);
  }

  void addLiveListObserver(TUILiveListObserver observer) {
    liveListManager.addObserver(observer);
  }

  void removeLiveListObserver(TUILiveListObserver observer) {
    liveListManager.removeObserver(observer);
  }

  void addIMFriendshipObserver(V2TimFriendshipListener observer) {
    friendshipManager.addFriendListener(listener: observer);
  }
}

extension VoiceRoomServiceWithRoom on VoiceRoomService {
  Future<TUIValueCallBack<TUIRoomInfo>> fetchRoomInfo() {
    return roomEngine.fetchRoomInfo();
  }

  Future<TUIValueCallBack<User>> fetchRoomOwnerInfo(String userId) async {
    final result = await roomEngine.getUserInfo(userId);
    return (result.code == TUIError.success && result.data != null)
        ? TUIValueCallBack(
        code: result.code,
        message: result.message,
        data: User.fromTUIUserInfo(result.data!))
        : TUIValueCallBack(code: result.code, message: result.message);
  }

  Future<TUIValueCallBack<TUILiveInfo>> fetchLiveInfo(String roomId) {
    return liveListManager.getLiveInfo(roomId);
  }

  Future<TUIActionCallback> setLiveInfo(String roomId,
      {String? coverUrl,
        String? backgroundUrl,
        List<int>? categoryList,
        bool? isPublicVisible,
        int? activityStatus}) {
    return liveListManager.setLiveInfo(roomId,
        backgroundUrl: backgroundUrl,
        coverUrl: coverUrl,
        categoryList: categoryList,
        isPublicVisible: isPublicVisible,
        activityStatus: activityStatus);
  }

  Future<TUIActionCallback> setRoomSeatModeByAdmin(TUISeatMode seatMode) {
    return roomEngine.updateRoomSeatModeByAdmin(seatMode);
  }
}

extension VoiceRoomServiceWithSeat on VoiceRoomService {
  Future<TUIValueCallBack<List<SeatInfo>>> fetchSeatList() async {
    final result = await roomEngine.getSeatList();
    if (result.code == TUIError.success && result.data != null) {
      final seatList =
      result.data!.map((seat) => SeatInfo.fromTUISeatInfo(seat)).toList();
      return TUIValueCallBack(
          code: result.code, message: result.message, data: seatList);
    }
    return TUIValueCallBack(code: result.code, message: result.message);
  }

  Future<TUIValueCallBack<List<SeatApplication>>>
  fetchSeatApplicationList() async {
    final result = await roomEngine.getSeatApplicationList();
    if (result.code == TUIError.success && result.data != null) {
      final seatApplications = result.data!
          .map((request) => SeatApplication.fromTUIRequest(request))
          .toList();
      return TUIValueCallBack(
          code: result.code, message: result.message, data: seatApplications);
    }
    return TUIValueCallBack(code: result.code, message: result.message);
  }
}

extension VoiceRoomServiceWithUser on VoiceRoomService {
  Future<TUIValueCallBack<User>> fetchUserInfo(String userId) async {
    final result = await roomEngine.getUserInfo(userId);
    return (result.code == TUIError.success && result.data != null)
        ? TUIValueCallBack(
        code: result.code,
        message: result.message,
        data: User.fromTUIUserInfo(result.data!))
        : TUIValueCallBack(code: result.code, message: result.message);
  }

  Future<TUIValueCallBack<List<User>>> fetchUserList() async {
    final result = await roomEngine.getUserList(0);
    if (result.code == TUIError.success && result.data != null) {
      final users = result.data!.userInfoList
          .map((userInfo) => User.fromTUIUserInfo(userInfo))
          .toList();
      return TUIValueCallBack(
          code: result.code, message: result.message, data: users);
    }
    return TUIValueCallBack(code: result.code, message: result.message);
  }

  Future<TUIActionCallback> followUser(String userId) async {
    final result = await friendshipManager.followUser(userIDList: [userId]);
    const success = 0;
    return result.code == success
        ? TUIActionCallback(code: TUIError.success, message: '')
        : TUIActionCallback(code: TUIError.errFailed, message: result.desc);
  }

  Future<TUIActionCallback> unfollowUser(String userId) async {
    final result = await friendshipManager.unfollowUser(userIDList: [userId]);
    const success = 0;
    return result.code == success
        ? TUIActionCallback(code: TUIError.success, message: '')
        : TUIActionCallback(code: TUIError.errFailed, message: result.desc);
  }

  Future<TUIValueCallBack<IMFollowType>> checkFollowType(String userId) async {
    final result =
    await friendshipManager.checkFollowType(userIDList: [userId]);
    const success = 0;
    if (result.code == success &&
        result.data != null &&
        result.data!.isNotEmpty) {
      final followType = IMFollowType.fromInt(result.data![0].followType ?? 0);
      return TUIValueCallBack(
          code: TUIError.success, message: '', data: followType);
    }
    return TUIValueCallBack(code: TUIError.errFailed, message: result.desc);
  }

  TUILoginUserInfo getSelfInfo() {
    return TUIRoomEngine.getSelfInfo();
  }
}