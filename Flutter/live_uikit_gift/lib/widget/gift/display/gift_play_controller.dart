import 'dart:math';

import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../../../common/constants/constants.dart';
import '../../../gift_define.dart';
import '../../../manager/cache/gift_cache_manager.dart';
import '../../../manager/index.dart';
import '../../../state/index.dart';

class GiftPlayController {
  final String roomId;
  final String language;
  late final GiftManager giftManager;
  late final LikeManager likeManager;
  OnReceiveGiftCallback? onReceiveGiftCallback;
  int _currentLikeAnimationCount = 0;

  GiftPlayController({required this.roomId,
    this.language = 'en',
    bool enablePreloading = false,
    OnGiftError? onError}) {
    giftManager = GiftManagerFactory.getGiftManager(roomId);
    likeManager = LikeManagerFactory.getLikeManager(roomId);
    giftManager.setCurrentLanguage(language);
    if (enablePreloading) {
      _preloadSvgaResource();
    }
    TUIGiftStore().onError = onError;
    _initCallback();
  }

  static void resetState() {
    TUIGiftStore().reset();
  }

  void dispose() {
    TUIGiftStore().reset();
    GiftManagerFactory.destroyGiftManager(roomId);
    LikeManagerFactory.destroyLikeManager(roomId);
  }
}

extension on GiftPlayController {
  void _preloadSvgaResource() async {
    debugPrint("GiftPlayController _preloadSvgaResource");
    List<String> svgaList = [
      "http://dldir1.qq.com/hudongzhibo/TRTC/TUIKit/Gift/svga/cat.svga",
      "http://dldir1.qq.com/hudongzhibo/TRTC/TUIKit/Gift/svga/car.svga",
      "http://dldir1.qq.com/hudongzhibo/TRTC/TUIKit/Gift/svga/sports_car.svga"
    ];
    for (String svgaUrl in svgaList) {
      await GiftCacheManager.getCachedFile(svgaUrl);
    }
  }

  void _initCallback() {
    giftManager.onReceiveGiftMessageCallback = (giftInfo, count, sender) {
      onReceiveGiftCallback?.call(giftInfo, count, sender);
      _playGift(roomId, giftInfo, count, sender);
    };

    giftManager.onReceiveLikeMessageCallback = (totalLikesReceived, sender) {
      final localLikeCount = TUIGiftStore().localLikeCount;
      final delta = totalLikesReceived - localLikeCount;
      if (delta < 0) {
        return;
      }

      final playCount = min(Constants.likeMaxAnimationCount, delta);
      TUIGiftStore().localLikeCount = totalLikesReceived;


      for (int i = 0; i < playCount; i++) {
        int delay = i * Constants.likeMaxAnimationIntervalMS;
        Future.delayed(Duration(milliseconds: delay), () {
          _playLike(roomId, sender);
        });
      }
    };
  }

  void _playGift(String roomId, TUIGiftInfo giftInfo, int giftCount, TUIUserInfo sender) {
    final giftData =
    TUIGiftData(giftCount: giftCount, giftInfo: giftInfo, sender: sender);
    final Map<String, TUIGiftData> newGiftData = {};
    TUIGiftStore()
        .giftDataMap
        .value
        .forEach((roomId, giftData) => {newGiftData[roomId] = giftData});
    newGiftData[roomId] = giftData;
    TUIGiftStore().giftDataMap.value = newGiftData;
  }

  void _playLike(String roomId, TUIUserInfo sender) {
    if (_currentLikeAnimationCount >= Constants.likeMaxAnimationCount) {
      return;
    }

    final likeData = TUILikeData(sender: sender);
    final Map<String, TUILikeData> newLikeData = {};
    TUIGiftStore()
        .likeDataMap
        .value
        .forEach((roomId, likeData) => {newLikeData[roomId] = likeData});
    newLikeData[roomId] = likeData;
    TUIGiftStore().likeDataMap.value = newLikeData;

    _currentLikeAnimationCount += 1;

    Future.delayed(const Duration(seconds: 3), () {
      _currentLikeAnimationCount -= 1;
    });
  }
}
