import 'package:flutter/foundation.dart';

import '../../common/widget/float_window/float_window_mode.dart';

class GlobalFloatWindowState {
  bool enableFloatWindowFeature = true;
  final ValueListenable<FloatWindowMode> floatWindowMode = ValueNotifier(FloatWindowMode.none);
  final ValueListenable<String> roomId = ValueNotifier("");
  final ValueListenable<String> ownerId = ValueNotifier("");
}
