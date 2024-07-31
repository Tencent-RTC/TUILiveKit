import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/state/live_define.dart';

class ViewState {
  final ValueNotifier<LinkStatus> linkStatus = ValueNotifier<LinkStatus>(LinkStatus.none);
  final ValueNotifier<bool> autoOpenCameraOnSeated = ValueNotifier<bool>(false);
  final ValueNotifier<LiveStatus> liveStatus = ValueNotifier<LiveStatus>(LiveStatus.none);

  void reset() {
    linkStatus.value = LinkStatus.none;
    autoOpenCameraOnSeated.value = false;
    liveStatus.value = LiveStatus.none;
  }
}
