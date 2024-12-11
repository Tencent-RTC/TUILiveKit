import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/manager/index.dart';
import 'package:tencent_live_uikit/state/index.dart';

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
    makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.livekit_room_destroy);
    viewState.liveStatus.value = LiveStatus.dashboard;
  }
}
