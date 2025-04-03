import 'package:flutter/material.dart';
import 'package:permission_handler/permission_handler.dart';

import '../../../common/index.dart';
import '../../../live_navigator_observer.dart';
import '../../manager/index.dart';
import 'audience/index.dart';

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
    _startForegroundService();
  }

  @override
  Widget build(BuildContext context) {
    return AudienceWidget(liveController: liveController);
  }

  @override
  void dispose() {
    liveController.destroy();
    _stopForegroundService();
    super.dispose();
  }

  void _initLiveController() async {
    liveController.seatController.seatState.setFilterEmptySeat(true);
    liveController.getRoomSate().roomId = widget.roomId;
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
