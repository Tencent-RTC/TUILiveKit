import 'dart:async';

import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

class LiveInfoState {
  String selfUserId = '';
  String roomId = '';
  final ValueNotifier<String> ownerId = ValueNotifier('');
  final ValueNotifier<String> ownerName = ValueNotifier('');
  final ValueNotifier<String> ownerAvatarUrl = ValueNotifier('');
  final ValueNotifier<int> fansNumber = ValueNotifier(0);
  final ValueNotifier<Set<TUIUserInfo>> followingList = ValueNotifier({});
  final StreamController<String> roomDismissedSubject = StreamController.broadcast();

  void dispose() {
    if (!roomDismissedSubject.isClosed) {
      roomDismissedSubject.close();
    }
  }
}
