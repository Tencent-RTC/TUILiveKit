import 'package:flutter/cupertino.dart';
import 'package:tencent_live_uikit/common/widget/float_window/float_window_mode.dart';

import 'global_float_window_state.dart';

/// Two scenarios:
/// 1.In-app : Overlay
/// 2.out-of-app : PIP
class GlobalFloatWindowManager {
  static final GlobalFloatWindowManager instance = GlobalFloatWindowManager._internal();

  final OverlayManager overlayManager = OverlayManager();
  final GlobalFloatWindowState state = GlobalFloatWindowState();

  factory GlobalFloatWindowManager() {
    return instance;
  }

  GlobalFloatWindowManager._internal();

  void setFloatWindowMode(FloatWindowMode mode) {
    if (state.floatWindowMode is ValueNotifier<FloatWindowMode>) {
      (state.floatWindowMode as ValueNotifier<FloatWindowMode>).value = mode;
    }
  }

  void enableFloatWindowFeature(bool enable) {
    state.enableFloatWindowFeature = enable;
  }

  bool isEnableFloatWindowFeature() {
    return state.enableFloatWindowFeature;
  }

  bool isFloating() {
    return state.floatWindowMode.value != FloatWindowMode.none;
  }

  void setRoomId(String roomId) {
    if (state.roomId is ValueNotifier<String>) {
      (state.roomId as ValueNotifier<String>).value = roomId;
    }
  }

  void setOwnerId(String userId) {
    if (state.ownerId is ValueNotifier<String>) {
      (state.ownerId as ValueNotifier<String>).value = userId;
    }
  }

  void switchToFullScreenMode() {
    overlayManager.switchToFullScreenMode();
  }
}

class OverlayManager {
  OverlayEntry? _overlayEntry;
  VoidCallback? _switchToFullScreenMode;

  void showOverlayEntry(OverlayEntry overlayEntry) {
    _overlayEntry = overlayEntry;
  }

  void closeOverlay() {
    if (_overlayEntry != null) {
      _overlayEntry!.remove();
      _overlayEntry = null;
      _switchToFullScreenMode = null;
      GlobalFloatWindowManager.instance.setRoomId("");
      GlobalFloatWindowManager.instance.setOwnerId("");
      GlobalFloatWindowManager.instance.setFloatWindowMode(FloatWindowMode.none);
    }
  }

  void switchToFullScreenMode() {
    _switchToFullScreenMode?.call();
  }

  void setSwitchToFullScreenCallback(VoidCallback callback) {
    _switchToFullScreenMode = callback;
  }
}
