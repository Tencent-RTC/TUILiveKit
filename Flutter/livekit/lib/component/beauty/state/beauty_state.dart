import 'package:flutter/material.dart';

class BeautyState {
  static const int defaultSmoothLevel = 0;
  static const int defaultWhitenessLevel = 0;
  static const int defaultRuddyLevel = 0;

  final ValueNotifier<int> smoothLevel = ValueNotifier<int>(defaultSmoothLevel);
  final ValueNotifier<int> whitenessLevel = ValueNotifier<int>(defaultWhitenessLevel);
  final ValueNotifier<int> ruddyLevel = ValueNotifier<int>(defaultRuddyLevel);

  void reset() {
    smoothLevel.value = defaultSmoothLevel;
    whitenessLevel.value = defaultWhitenessLevel;
    ruddyLevel.value = defaultRuddyLevel;
  }
}
