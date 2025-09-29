import 'package:rtc_room_engine/rtc_room_engine.dart';

class TUILikeData {
  TUIUserInfo sender = TUIUserInfo(
      userId: '', userName: '', avatarUrl: '', userRole: TUIRole.generalUser);

  TUILikeData({TUIUserInfo? sender}) {
    if (sender != null) {
      this.sender = sender;
    }
  }

  @override
  String toString() {
    return 'TUILikeData{sender: $sender}';
  }
}