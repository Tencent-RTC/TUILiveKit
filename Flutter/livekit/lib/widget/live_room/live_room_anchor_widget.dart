import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/manager/live_controller.dart';
import 'package:tencent_live_uikit/widget/live_room/anchor/index.dart';

class TUILiveRoomAnchorWidget extends StatefulWidget {
  final String roomId;

  const TUILiveRoomAnchorWidget({super.key, required this.roomId});

  @override
  State<StatefulWidget> createState() {
    return TUILiveRoomAnchorWidgetState();
  }
}

class TUILiveRoomAnchorWidgetState extends State<TUILiveRoomAnchorWidget> {
  final LiveController liveController = LiveController();

  @override
  void initState() {
    super.initState();
    _initLiveController();
  }

  @override
  Widget build(BuildContext context) {
    return AnchorWidget(liveController: liveController);
  }

  @override
  void dispose() {
    AudioEffectStateFactory.removeState(widget.roomId);
    MusicStateFactory.removeState(widget.roomId);
    liveController.destroy();
    super.dispose();
  }

  void _initLiveController() {
    liveController.getSeatState().setFilterEmptySeat(true);
    liveController.setRoomId(widget.roomId);
    TUISeatMode seatMode = TUISeatMode.applyToTake;
    liveController.roomController.initCreateRoomState(widget.roomId, "", seatMode, Constants.defaultMaxSeatCount);
    liveController.roomController.startPreview();
  }
}
