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
    _service.setBeautyLevel(beautyLevel);
    state.smoothLevel.value = beautyLevel;
  }

  void setWhitenessLevel(int whitenessLevel) {
    _service.setWhitenessLevel(whitenessLevel);
    state.whitenessLevel.value = whitenessLevel;
  }

  void setRuddyLevel(int ruddyLevel) {
    _service.setRuddyLevel(ruddyLevel);
    state.ruddyLevel.value = ruddyLevel;
  }

  void closeBeautyEffect() {
    setBeautyLevel(0);
    setWhitenessLevel(0);
    setRuddyLevel(0);
  }

  void resetToDefaultBeautyEffect() {
    state.reset();

    _service.setBeautyLevel(state.smoothLevel.value);
    _service.setWhitenessLevel(state.whitenessLevel.value);
    _service.setRuddyLevel(state.ruddyLevel.value);
  }
}

extension on BeautyManager {
  void _subscribeStateRemoval() {
    BeautyStateFactory.addRemovalListener(
        BeautyManager.beautyStateKey, _handleStateRemoved);
  }

  void _handleStateRemoved() {
    _dispose();
  }

  void _dispose() {
    resetToDefaultBeautyEffect();
    BeautyStateFactory.removeRemovalListener(BeautyManager.beautyStateKey);
  }
}
