import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/manager/live_controller.dart';
import 'package:tencent_live_uikit/widget/live_room/audience/index.dart';

class TUILiveRoomAudienceWidget extends StatefulWidget {
  final String roomId;

  const TUILiveRoomAudienceWidget({super.key, required this.roomId});

  @override
  State<StatefulWidget> createState() {
    return TUILiveRoomAudienceWidgetState();
  }
}

class TUILiveRoomAudienceWidgetState extends State<TUILiveRoomAudienceWidget> {
  final LiveController liveController = LiveController();

  @override
  void initState() {
    super.initState();
    _initLiveController();
  }

  @override
  Widget build(BuildContext context) {
    return AudienceWidget(liveController: liveController);
  }

  @override
  void dispose() {
    liveController.destroy();
    super.dispose();
  }

  void _initLiveController() async {
    liveController.seatController.seatState.setFilterEmptySeat(true);
    liveController.getRoomSate().roomId = widget.roomId;
  }
}
