import 'package:tencent_live_uikit/component/beauty/state/beautyStateFactory.dart';

import '../api/beauty_service.dart';
import '../state/beauty_state.dart';

class BeautyManager {
  static const beautyStateKey = 'liveStream';
  final BeautyService _service = BeautyService();
  final BeautyState state;

  BeautyManager({required this.state}) {
    _subscribeStateRemoval();
  }

  void setBeautyLevel(int beautyLevel) {
    state.smoothLevel.value = beautyLevel;
    _service.setBeautyStyle(state.smoothLevel.value, state.whitenessLevel.value, state.ruddyLevel.value);
  }

  void setWhitenessLevel(int whitenessLevel) {
    state.whitenessLevel.value = whitenessLevel;
    _service.setBeautyStyle(state.smoothLevel.value, state.whitenessLevel.value, state.ruddyLevel.value);
  }

  void setRuddyLevel(int ruddyLevel) {
    state.ruddyLevel.value = ruddyLevel;
    _service.setBeautyStyle(state.smoothLevel.value, state.whitenessLevel.value, state.ruddyLevel.value);
  }

  void closeBeautyEffect() {
    setBeautyLevel(0);
    setWhitenessLevel(0);
    setRuddyLevel(0);
  }

  void resetToDefaultBeautyEffect() {
    state.reset();
    _service.setBeautyStyle(state.smoothLevel.value, state.whitenessLevel.value, state.ruddyLevel.value);
  }
}

extension on BeautyManager {
  void _subscribeStateRemoval() {
    BeautyStateFactory.addRemovalListener(BeautyManager.beautyStateKey, _handleStateRemoved);
  }

  void _handleStateRemoved() {
    _dispose();
  }

  void _dispose() {
    resetToDefaultBeautyEffect();
    BeautyStateFactory.removeRemovalListener(BeautyManager.beautyStateKey);
  }
}
