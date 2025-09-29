import 'dart:async';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:live_uikit_barrage/widget/display/barrage_display_controller.dart';
import 'package:live_uikit_gift/widget/gift/display/gift_play_controller.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/component/index.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

import '../../component/beauty/state/beautyStateFactory.dart';
import '../../live_navigator_observer.dart';
import '../manager/live_stream_manager.dart';
import 'audience/audience_widget.dart';

class TUILiveRoomAudienceWidget extends StatefulWidget {
  final String roomId;

  const TUILiveRoomAudienceWidget({super.key, required this.roomId});

  @override
  State<TUILiveRoomAudienceWidget> createState() => _TUILiveRoomAudienceWidgetState();
}

class _TUILiveRoomAudienceWidgetState extends State<TUILiveRoomAudienceWidget> {
  LiveCoreController? _liveCoreController;
  LiveStreamManager? _liveStreamManager;
  StreamSubscription? _toastSubscription;

  @override
  void initState() {
    super.initState();
    LiveDataReporter.reportComponent(LiveComponentType.liveRoom);
    _changeStatusBar2LightMode();
    _init();
    _addObserver();
    _startWakeLock();
  }

  @override
  void dispose() {
    _stopWakeLock();
    _removeObserver();
    _dispose();
    BeautyStateFactory.removeState(BeautyManager.beautyStateKey);
    BarrageDisplayController.resetState();
    GiftPlayController.resetState();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      resizeToAvoidBottomInset: false,
      body: AudienceWidget(
        roomId: widget.roomId,
        liveCoreController: _liveCoreController!,
        liveStreamManager: _liveStreamManager!,
      ),
    );
  }
}

extension on _TUILiveRoomAudienceWidgetState {
  void _changeStatusBar2LightMode() {
    SystemChrome.setSystemUIOverlayStyle(SystemUiOverlayStyle.light);
  }

  void _init() {
    _startForegroundService();
    _liveCoreController ??= LiveCoreController();
    _liveStreamManager ??=
        LiveStreamManager(provider: CoreStateProvider(getCoreState: _liveCoreController!.getCoreState));
    _toastSubscription = _liveStreamManager?.toastSubject.stream.listen((toast) => makeToast(msg: toast));
    _liveCoreController?.registerConnectionObserver(_liveStreamManager!.liveStreamObserver);
  }

  void _dispose() {
    _stopForegroundService();
    _liveCoreController?.unregisterConnectionObserver(_liveStreamManager!.liveStreamObserver);
    _toastSubscription?.cancel();
    _liveStreamManager?.dispose();
    _liveCoreController?.dispose();
  }

  void _startForegroundService() async {
    String description = LiveKitLocalizations.of(TUILiveKitNavigatorObserver.instance.getContext())!.common_app_running;

    final hasCameraPermission = await Permission.camera.status == PermissionStatus.granted;
    if (!hasCameraPermission) {
      LiveKitLogger.error(
          '[ForegroundService] failed to start video foreground service. reason: without camera permission');
      return;
    }
    TUILiveKitPlatform.instance.startForegroundService(ForegroundServiceType.video, "", description);
  }

  void _stopForegroundService() {
    TUILiveKitPlatform.instance.stopForegroundService(ForegroundServiceType.video);
    Permission.camera.onGrantedCallback(null);
  }

  void _addObserver() {
    if (_liveStreamManager == null) {
      return;
    }
    _liveCoreController?.registerConnectionObserver(_liveStreamManager!.liveStreamObserver);
    _liveCoreController?.registerBattleObserver(_liveStreamManager!.battleManagerObserver);
  }

  void _removeObserver() {
    if (_liveStreamManager == null) {
      return;
    }
    _liveCoreController?.unregisterConnectionObserver(_liveStreamManager!.liveStreamObserver);
    _liveCoreController?.unregisterBattleObserver(_liveStreamManager!.battleManagerObserver);
  }

  void _startWakeLock() {
    TUILiveKitPlatform.instance.enableWakeLock(true);
  }

  void _stopWakeLock() {
    TUILiveKitPlatform.instance.enableWakeLock(false);
  }
}
