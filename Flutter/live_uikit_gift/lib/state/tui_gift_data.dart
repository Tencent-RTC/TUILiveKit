import 'package:rtc_room_engine/rtc_room_engine.dart';

class TUIGiftData {
  int giftCount = 0;
  TUIGiftInfo giftInfo = TUIGiftInfo.fromJson({});
  TUIUserInfo sender = TUIUserInfo(
      userId: '', userName: '', avatarUrl: '', userRole: TUIRole.generalUser);

  TUIGiftData({int? giftCount, TUIGiftInfo? giftInfo, TUIUserInfo? sender}) {
    if (giftCount != null) {
      this.giftCount = giftCount;
    }
    if (giftInfo != null) {
      this.giftInfo = giftInfo;
    }
    if (sender != null) {
      this.sender = sender;
    }
  }

  @override
  String toString() {
    return 'TUIGiftData{giftCount: $giftCount, giftInfo: $giftInfo, sender: $sender}';
  }
}