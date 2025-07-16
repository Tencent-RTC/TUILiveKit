import 'dart:ui';

import 'beauty_state.dart';

class BeautyStateFactory {
  static final Map<String, BeautyState> _stateMap = {};
  static final Map<String, VoidCallback> _listeners = {};

  static BeautyState getState(String roomId) {
    if (_stateMap.containsKey(roomId)) {
      return _stateMap[roomId]!;
    }
    final state = BeautyState();
    _stateMap[roomId] = state;
    return state;
  }

  static void removeState(String roomId) {
    _listeners[roomId]?.call();
    _listeners.remove(roomId);
    _stateMap.remove(roomId);
  }

  static void addRemovalListener(String roomId, VoidCallback onRemoved) {
    _listeners[roomId] = onRemoved;
  }

  static void removeRemovalListener(String roomId) {
    _listeners.remove(roomId);
  }
}
