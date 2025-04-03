import 'package:flutter/material.dart';

import 'gift_message.dart';

class GiftState {
  ValueNotifier<GiftMessage> giftMessage = ValueNotifier(GiftMessage());
  final ValueNotifier<int> showLikeStart = ValueNotifier(0);
}
