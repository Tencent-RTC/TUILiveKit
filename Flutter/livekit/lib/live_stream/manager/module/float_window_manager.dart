import 'dart:convert';

import 'package:flutter/cupertino.dart';

import '../../../common/widget/float_window/float_window_mode.dart';
import '../../state/float_window_state.dart';
import '../live_stream_manager.dart';

class FloatWindowManager {
  final LSFloatWindowState floatWindowState = LSFloatWindowState();

  late final Context context;

  late final VoidCallback _onPipModeChangedListener = _onPipModeChanged;
  late final VoidCallback _onFloatWindowModeChangedListener = _onFloatWindowModeChanged;

  void init(Context context) {
    this.context = context;
    context.coreLayoutState.pipMode.addListener(_onPipModeChangedListener);
    floatWindowState.floatWindowMode.addListener(_onFloatWindowModeChangedListener);
  }

  void dispose() {
    context.coreLayoutState.pipMode.removeListener(_onPipModeChangedListener);
    floatWindowState.floatWindowMode.removeListener(_onFloatWindowModeChangedListener);
  }

  void enablePipMode(bool enable) {
    if (floatWindowState.enablePipMode is ValueNotifier<bool>) {
      ValueNotifier<bool> _enablePipMode = floatWindowState.enablePipMode as ValueNotifier<bool>;
      _enablePipMode.value = enable;
    }
  }

  String buildEnablePipJsonParams(bool enable, String roomId) {
    Map<String, dynamic> jsonObject = {
      'api': 'enablePictureInPicture',
      'params': {
        "room_id": roomId,
        "enable": enable,
        "camBackgroundCapture": true,
        "canvas": {"width": 720, "height": 1280, "backgroundColor": "#000000"},
        "regions": [
          {
            "userId": "",
            "userName": "",
            "width": 1.0,
            "height": 1.0,
            "x": 0.0,
            "y": 0.0,
            "streamType": "high",
            "backgroundColor": "#000000",
            "backgroundImage": "" // /path/to/user1_placeholder.png
          }
        ]
      }
    };
    final jsonString = jsonEncode(jsonObject);
    return jsonString;
  }

  void _onPipModeChanged() {
    if (floatWindowState.floatWindowMode is ValueNotifier<FloatWindowMode>) {
      (floatWindowState.floatWindowMode as ValueNotifier<FloatWindowMode>).value =
          context.coreLayoutState.pipMode.value ? FloatWindowMode.outOfApp : FloatWindowMode.none;
    }
  }

  void _onFloatWindowModeChanged() {
    if (floatWindowState.isFloatWindowMode is ValueNotifier<bool>) {
      (floatWindowState.isFloatWindowMode as ValueNotifier<bool>).value =
          floatWindowState.floatWindowMode.value != FloatWindowMode.none;
    }
  }
}
