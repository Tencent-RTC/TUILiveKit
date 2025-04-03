import 'package:flutter/material.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

import '../../../common/index.dart';
import '../../../live_navigator_observer.dart';
import '../../manager/index.dart';
import 'anchor/index.dart';

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
    _startForegroundService();
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
    _stopForegroundService();
    super.dispose();
  }

  void _initLiveController() {
    liveController.getSeatState().setFilterEmptySeat(true);
    liveController.setRoomId(widget.roomId);
    TUISeatMode seatMode = TUISeatMode.applyToTake;
    liveController.roomController.initCreateRoomState(
        widget.roomId, "", seatMode, Constants.defaultMaxSeatCount);
    liveController.roomController.startPreview();
  }

  void _startForegroundService() {
    String description = LiveKitLocalizations.of(
            TUILiveKitNavigatorObserver.instance.getContext())!
        .live_app_running;
    Permission.camera.onGrantedCallback(() {
      TUILiveKitPlatform.instance
          .startForegroundService(ForegroundServiceType.video, "", description);
    });
  }

  void _stopForegroundService() {
    TUILiveKitPlatform.instance
        .stopForegroundService(ForegroundServiceType.video);
    Permission.camera.onGrantedCallback(null);
  }
}
