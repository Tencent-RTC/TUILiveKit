import 'dart:async';

import 'package:flutter/cupertino.dart';
import 'package:live_uikit_gift/manager/index.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../../../common/constants/constants.dart';
import '../../../state/index.dart';

class PendingCount {
  int _count = 0;

  void increment() {
    _count++;
  }

  int getCount() {
    return _count;
  }

  void reset() {
    _count = 0;
  }
}

class LikeSendController {
  final String roomId;
  late LikeManager likeManager;

  final int _minSendInterval = 6;
  int _lastSendTime = 0;
  Timer? _afterSendLikeTimer;
  final PendingCount _pendingCount = PendingCount();

  LikeSendController({required this.roomId}) {
    likeManager = LikeManagerFactory.getLikeManager(roomId);
  }

  void dispose() {
    _afterSendLikeTimer?.cancel();
  }

  void sendLike() {
    _showLikeAnimation();

    _pendingCount.increment();

    final now = DateTime.now().millisecondsSinceEpoch ~/ 1000;
    final elapsed = now - _lastSendTime;
    _afterSendLikeTimer?.cancel();

    if (elapsed >= _minSendInterval) {
      _sendLikeInternal();
      _lastSendTime = DateTime.now().millisecondsSinceEpoch ~/ 1000;
    } else {
      _afterSendLikeTimer = Timer(
        Duration(seconds: _minSendInterval - elapsed),
            () {
          _sendLikeInternal();
          _lastSendTime = DateTime.now().millisecondsSinceEpoch ~/ 1000;
        },
      );
    }
  }

  Future<TUIValueCallBack<int>> getLikesCount() {
    return likeManager.getLikesCount();
  }
}

extension on LikeSendController {
  void _sendLikeInternal() {
    final countToSend = _pendingCount.getCount();
    if (countToSend <= 0) return;

    try {
      debugPrint("sendLike count: $countToSend");
      likeManager.sendLike(countToSend);
      _pendingCount.reset();
    } catch (e) {
      debugPrint("sendLike failed: $e");
    }
  }

  void _showLikeAnimation() {
    TUIGiftStore().localLikeCount += 1;
    TUIGiftStore().showLikeStart.value = TUIGiftStore().localLikeCount;

    final selfInfo = TUIRoomEngine.getSelfInfo();
    final sender = TUIUserInfo(
        userId: selfInfo.userId,
        userName: selfInfo.userName ?? '',
        avatarUrl: selfInfo.avatarUrl ?? Constants.defaultAvatar,
        userRole: TUIRole.generalUser);
    TUIGiftStore().likeDataMap.value[roomId] = TUILikeData(sender: sender);
  }
}
