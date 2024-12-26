import 'package:flutter/cupertino.dart';
import 'package:gift/state/gift_store.dart';
import 'package:gift/state/gift_user.dart';

class LikeSendController {
  final int _maxLikeCount = 20;
  final int _minDuration = 5;
  int _currentLikeCount = 0;
  int _lastSendLikeTime = 0;

  LikeSendController({required String roomId, required GiftUser owner, required GiftUser self}) {
    GiftStore().likeManager.init(roomId, owner, self);
  }

  void sendLikeMessage() {
    debugPrint("LikeController sendLikeMessage");
    if (_currentLikeCount >= _maxLikeCount) {
      GiftStore().likeManager.sendLike();
      _currentLikeCount = 0;
      _lastSendLikeTime = DateTime.now().millisecondsSinceEpoch ~/ 1000;
    }

    int currentTime = DateTime.now().millisecondsSinceEpoch ~/ 1000;
    debugPrint("LikeController currentTime:{$currentTime}");
    if (currentTime - _lastSendLikeTime > _minDuration) {
      GiftStore().likeManager.sendLike();
      _currentLikeCount = 0;
      _lastSendLikeTime = DateTime.now().millisecondsSinceEpoch ~/ 1000;
    } else {
      _currentLikeCount += 1;
      showLikeAnimation();
    }
  }

  void showLikeAnimation() {
    GiftStore().state.showLikeStart.value = _currentLikeCount + 1;
  }
}
