import 'package:tencent_live_uikit/common/ui_component/music/store/music_state.dart';

class MusicStateFactory {
  static final Map<String, MusicState> _stateMap = {};

  static MusicState getState(String roomId) {
    if (_stateMap.containsKey(roomId)) {
      return _stateMap[roomId]!;
    }
    final state = MusicState();
    _stateMap[roomId] = state;
    return state;
  }

  static void removeState(String roomId) {
    _stateMap.remove(roomId);
  }
}
