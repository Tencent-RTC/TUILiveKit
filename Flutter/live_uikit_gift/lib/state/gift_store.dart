import '../manager/index.dart';
import 'gift_message.dart';

import 'gift_state.dart';
import 'gift_user.dart';

typedef OnGiftError = void Function(int code, String message);

class GiftStore {
  static GiftStore? _instance;
  OnGiftError? onError;

  GiftStore._internal();

  factory GiftStore() {
    _instance ??= GiftStore._internal();
    return _instance!;
  }

  GiftState state = GiftState();

  GiftManager giftManager = GiftManager();
  LikeManager likeManager = LikeManager();

  String roomId = "";

  GiftUser selfInfo = GiftUser();

  GiftUser ownerInfo = GiftUser();

  List<GiftModel> giftModelList = [];

  void init(String roomId, GiftUser owner, GiftUser self) {
    if (this.roomId != roomId ||
        selfInfo.userId != self.userId ||
        ownerInfo.userId != owner.userId) {
      this.roomId = roomId;
      ownerInfo = owner;
      selfInfo = self;
    } else {
      ownerInfo.updateUserInfo(owner);
      selfInfo.updateUserInfo(self);
    }
  }
}
