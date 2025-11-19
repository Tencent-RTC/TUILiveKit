import 'package:flutter/foundation.dart';

class FloatWindowController {
  final ValueChanged<bool> onTapSwitchFloatWindowInApp;
  final ValueChanged<bool> onSwitchFloatWindowOutOfApp;
  final ValueListenable<bool> isFullScreen = ValueNotifier(true);

  FloatWindowController({required this.onTapSwitchFloatWindowInApp, required this.onSwitchFloatWindowOutOfApp});

  void switchFloatWindowInApp(bool isFullScreen) {
    if (this.isFullScreen is ValueNotifier<bool>) {
      (this.isFullScreen as ValueNotifier<bool>).value = isFullScreen;
    }
  }
}
