import 'package:flutter/material.dart';
import 'package:gift/state/index.dart';

class GiftSendController {
  late PageController pageController = PageController();

  GiftSendController({required String roomId, required GiftUser owner, required GiftUser self}) {
    GiftStore().giftManager.init(roomId, owner, self);
  }

  Future<bool> sendGift(GiftMessage message) async {
    return GiftStore().giftManager.sendGift(message);
  }

  void setGiftList(List<GiftModel> giftList) {
    if (giftList.isNotEmpty) {
      GiftStore().giftModelList.clear();
      GiftStore().giftModelList.addAll(giftList);
    }
  }
}
