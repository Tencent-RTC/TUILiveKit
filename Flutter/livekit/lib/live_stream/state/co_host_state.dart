import 'package:flutter/cupertino.dart';
import 'package:rtc_room_engine/api/extension/tui_live_connection_manager.dart';

class LSCoHostState {
  String currentRoomId = '';
  final ValueNotifier<List<TUIConnectionUser>> connectedUsers = ValueNotifier([]);
  final ValueNotifier<List<TUIConnectionUser>> recommendedUsers = ValueNotifier([]);
  final ValueNotifier<String> recommendListCursor = ValueNotifier('');
}

enum TUIConnectionStatus { none, inviting, connected }

final _connectionStatus = Expando<TUIConnectionStatus>();

extension TUIConnectionUserWithStatus on TUIConnectionUser {
  TUIConnectionStatus? get connectionStatus => _connectionStatus[this];

  set connectionStatus(TUIConnectionStatus? status) =>
      _connectionStatus[this] = status;
}
