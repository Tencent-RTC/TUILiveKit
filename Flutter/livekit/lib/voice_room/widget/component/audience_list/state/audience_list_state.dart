import 'dart:async';

import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

const int maxShowUserCount = 100;

class AudienceListState {
  String roomId = '';
  String ownerId = '';
  final ValueNotifier<int> audienceCount = ValueNotifier(0);
  final ValueNotifier<List<TUIUserInfo>> audienceList = ValueNotifier([]);
  final StreamController<String> roomDismissedSubject = StreamController.broadcast();

  void dispose() {
    if (!roomDismissedSubject.isClosed) {
      roomDismissedSubject.close();
    }
  }
}