import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';

import 'package:rtc_room_engine/rtc_room_engine.dart';

typedef OnReceiveGiftMessageCallback = void Function(
    TUIGiftInfo giftInfo, int count, TUIUserInfo sender);
typedef OnReceiveLikeMessageCallback = void Function(
    int totalLikesReceived, TUIUserInfo sender);

class GiftManagerFactory {
  static final Map<String, GiftManager> _giftManagerMap = {};

  static GiftManager getGiftManager(String roomId) {
    if (_giftManagerMap.containsKey(roomId)) {
      return _giftManagerMap[roomId]!;
    }

    final giftManager = GiftManager(roomId: roomId);
    _giftManagerMap[roomId] = giftManager;
    return giftManager;
  }

  static void destroyGiftManager(String roomId) {
    _giftManagerMap.remove(roomId);
  }
}

class GiftManager extends TUILiveGiftObserver {
  final String roomId;
  OnReceiveGiftMessageCallback? onReceiveGiftMessageCallback;
  OnReceiveLikeMessageCallback? onReceiveLikeMessageCallback;

  final TUILiveGiftManager _giftManager = TUIRoomEngine.sharedInstance()
      .getExtension(TUIExtensionType.liveGiftManager);

  GiftManager(
      {required this.roomId,
      this.onReceiveGiftMessageCallback,
      this.onReceiveLikeMessageCallback}) {
    _addObserver();

    super.onReceiveGiftMessage = (roomId, giftInfo, count, sender) {
      if (roomId != this.roomId) {
        return;
      }
      onReceiveGiftMessageCallback?.call(giftInfo, count, sender);
    };

    super.onReceiveLikesMessage = (roomId, totalLikesReceived, sender) {
      if (roomId != this.roomId) {
        return;
      }
      onReceiveLikeMessageCallback?.call(totalLikesReceived, sender);
    };
  }

  void dispose() {
    _removeObserver();
  }

  Future<TUIActionCallback> sendGift(TUIGiftInfo giftInfo, int count) {
    return _giftManager.sendGift(roomId, giftInfo.giftId, count);
  }

  void setCurrentLanguage(String language) async {
    Map<String, dynamic> params = {'language': language};
    Map<String, dynamic> jsonObject = {
      'api': 'setCurrentLanguage',
      'params': params
    };

    try {
      final jsonString = json.encode(jsonObject);
      final result = await TUIRoomEngine.sharedInstance().invokeExperimentalAPI(jsonString);
    } catch (e) {
      debugPrint('setCurrentLanguage failed.');
    }
  }

  Future<TUIActionCallback> getGiftList() async {
    final result = await _giftManager.getGiftList(roomId);
    if (result.code != TUIError.success || result.data == null) {
      return TUIActionCallback(code: result.code, message: result.message);
    }
    final List<TUIGiftCategory> giftCategoryList = result.data!;
    List<TUIGiftInfo> giftList = List.empty(growable: true);
    for (final giftCategory in giftCategoryList) {
      giftList.addAll(giftCategory.giftList);
    }
    _updateGiftListMap(roomId, giftList);

    return TUIActionCallback(code: result.code, message: result.message);
  }

  Future<TUIValueCallBack<TUIGiftCountRequestResult>> getGiftCountByAnchor() {
    return _giftManager.getGiftCountByAnchor(roomId);
  }
}

extension on GiftManager {
  void _addObserver() {
    _giftManager.addObserver(this);
  }

  void _removeObserver() {
    _giftManager.removeObserver(this);
  }

  void _updateGiftListMap(String roomId, List<TUIGiftInfo> giftList) {
    final Map<String, List<TUIGiftInfo>> newGiftListMap = {};
    TUIGiftStore()
        .giftListMap
        .value
        .forEach((roomId, giftList) => {newGiftListMap[roomId] = giftList});
    newGiftListMap[roomId] = giftList;
    TUIGiftStore().giftListMap.value = newGiftListMap;
  }
}
