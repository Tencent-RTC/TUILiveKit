import 'package:rtc_room_engine/rtc_room_engine.dart';

typedef OnReceiveGiftCallback = void Function(
    TUIGiftInfo giftInfo, int count, TUIUserInfo sender);

typedef OnSendGiftCallback = void Function(TUIGiftInfo giftInfo, int count);
