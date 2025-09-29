import 'package:rtc_room_engine/rtc_room_engine.dart';

class LikeManagerFactory {
  static final Map<String, LikeManager> _likeManagerMap = {};

  static LikeManager getLikeManager(String roomId) {
    if (_likeManagerMap.containsKey(roomId)) {
      return _likeManagerMap[roomId]!;
    }

    final giftManager = LikeManager(roomId: roomId);
    _likeManagerMap[roomId] = giftManager;
    return giftManager;
  }

  static void destroyLikeManager(String roomId) {
    _likeManagerMap.remove(roomId);
  }
}

class LikeManager {
  final String roomId;
  final TUILiveGiftManager _giftManager = TUIRoomEngine.sharedInstance()
      .getExtension(TUIExtensionType.liveGiftManager);

  LikeManager({required this.roomId});

  Future<TUIActionCallback> sendLike(int count) {
    return _giftManager.sendLike(roomId, count);
  }

  Future<TUIValueCallBack<int>> getLikesCount() {
    return _giftManager.getLikesCount(roomId);
  }
}
