import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';

class RoomListState {
  final ValueNotifier<List<TUILiveInfo>> liveInfoList = ValueNotifier<List<TUILiveInfo>>([]);
  final ValueNotifier<bool> refreshStatus = ValueNotifier<bool>(false);
  final ValueNotifier<bool> loadStatus = ValueNotifier<bool>(false);
  final ValueNotifier<bool> isHaveMoreData = ValueNotifier<bool>(false);
  String cursor = "";
}
