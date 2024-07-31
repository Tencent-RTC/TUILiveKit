import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/ui_component/room_list/widget/room_list_widget.dart';
import 'package:tencent_live_uikit/manager/live_controller.dart';

class LiveListWidget extends StatelessWidget {
  late final LiveController _liveController;

  LiveListWidget({super.key}) {
    _initLiveController();
  }

  @override
  Widget build(BuildContext context) {
    return RoomListWidget(liveController: _liveController);
  }

  void _initLiveController() {
    _liveController = LiveController();
  }
}
