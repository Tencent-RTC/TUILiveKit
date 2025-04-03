
import 'audio_effect_state.dart';

class AudioEffectStateFactory {
  static final Map<String, AudioEffectState> _stateMap = {};

  static AudioEffectState getState(String roomId) {
    if (_stateMap.containsKey(roomId)) {
      return _stateMap[roomId]!;
    }
    final state = AudioEffectState();
    _stateMap[roomId] = state;
    return state;
  }

  static void removeState(String roomId) {
    _stateMap.remove(roomId);
  }
}
