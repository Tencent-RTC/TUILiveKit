import 'package:flutter/material.dart';

class BeautyState {
  final ValueNotifier<int> smoothLevel = ValueNotifier<int>(6);
  final ValueNotifier<int> whitenessLevel = ValueNotifier<int>(6);
  final ValueNotifier<int> ruddyLevel = ValueNotifier<int>(6);
  final ValueNotifier<bool> enableBeauty = ValueNotifier(false);
  final ValueNotifier<int> xmagicType = ValueNotifier(0);

  void reset() {
    smoothLevel.value = 6;
    whitenessLevel.value = 6;
    ruddyLevel.value = 6;
  }
}
