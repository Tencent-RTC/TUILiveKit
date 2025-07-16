import 'package:flutter/cupertino.dart';

import '../../../gift_define.dart';
import '../../../manager/cache/gift_cache_manager.dart';
import '../../../state/index.dart';

class GiftDisplayController {
  GiftDisplayController(
      {required String roomId,
      required GiftUser owner,
      required GiftUser self,
      bool enablePreloading = false,
      OnGiftError? onError}) {
    GiftStore().giftManager.init(roomId, owner, self);
    GiftStore().likeManager.init(roomId, owner, self);
    if (enablePreloading) {
      _preloadSvgaResource();
    }
    GiftStore().onError = onError;
  }

  void setGiftCallback(
      {OnReceiveGiftCallback? onReceiveGiftCallback,
      OnSendGiftCallback? onSendGiftCallback}) {
    if (onReceiveGiftCallback != null) {
      GiftStore().giftManager.setOnReceiveGiftCallback(onReceiveGiftCallback);
    }
    if (onSendGiftCallback != null) {
      GiftStore().giftManager.setOnSendGiftCallback(onSendGiftCallback);
    }
  }

  void _preloadSvgaResource() async {
    debugPrint("GiftDisplayController _preloadSvgaResource");
    List<String> svgaList = [
      "http://dldir1.qq.com/hudongzhibo/TRTC/TUIKit/Gift/svga/cat.svga",
      "http://dldir1.qq.com/hudongzhibo/TRTC/TUIKit/Gift/svga/car.svga",
      "http://dldir1.qq.com/hudongzhibo/TRTC/TUIKit/Gift/svga/sports_car.svga"
    ];
    for (String svgaUrl in svgaList) {
      await GiftCacheManager.getCachedFile(svgaUrl);
    }
  }

  static void resetState() {
    GiftStore().state.giftMessage.value = GiftMessage();
    GiftStore().state.showLikeStart.value = 0;
  }
}
