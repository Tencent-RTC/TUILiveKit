import 'package:flutter/material.dart';

import '../../../common/index.dart';
import '../../state/index.dart';
import '../index.dart';

class ViewController extends Controller {
  ViewController(super.state, super.service);

  @override
  void destroy() {}

  void finish() {
    Navigator.pop(Global.appContext());
  }

  void enableAutoOpenCameraOnSeated(bool enable) {
    viewState.autoOpenCameraOnSeated.value = enable;
  }

  void updateLiveStatus(LiveStatus status) {
    viewState.liveStatus.value = status;
  }

  void onLiveEnd(String roomId) {
    makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.live_room_destroy);
    viewState.liveStatus.value = LiveStatus.dashboard;
  }
}
