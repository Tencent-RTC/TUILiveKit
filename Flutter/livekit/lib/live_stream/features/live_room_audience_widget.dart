import 'dart:async';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:live_uikit_barrage/widget/display/barrage_display_controller.dart';
import 'package:live_uikit_gift/widget/gift/display/gift_play_controller.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/component/float_window/global_float_window_manager.dart';
import 'package:tencent_live_uikit/component/index.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

import '../../common/widget/float_window/float_window_controller.dart';
import '../../common/widget/float_window/float_window_mode.dart';
import '../../component/beauty/state/beautyStateFactory.dart';
import '../manager/live_stream_manager.dart';
import 'audience/audience_widget.dart';

class TUILiveRoomAudienceWidget extends StatefulWidget {
  final String roomId;
  final FloatWindowController? floatWindowController;

  const TUILiveRoomAudienceWidget({super.key, required this.roomId, this.floatWindowController});

  @override
  State<TUILiveRoomAudienceWidget> createState() => _TUILiveRoomAudienceWidgetState();
}

class _TUILiveRoomAudienceWidgetState extends State<TUILiveRoomAudienceWidget> {
  LiveCoreController? _liveCoreController;
  LiveStreamManager? _liveStreamManager;
  StreamSubscription? _toastSubscription;
  late final VoidCallback _onFloatWindowModeChangedListener = _onFloatWindowModeChanged;
  late final VoidCallback _onFullScreenChangedListener = _onFullScreenChanged;

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
        onTapEnterFloatWindowInApp: () {widget.floatWindowController?.onTapSwitchFloatWindowInApp(true);},
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
  }

  void _dispose() {
    _stopForegroundService();
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
    _liveStreamManager!.floatWindowState.floatWindowMode.addListener(_onFloatWindowModeChangedListener);
    widget.floatWindowController?.isFullScreen.addListener(_onFullScreenChangedListener);
  }

  void _removeObserver() {
    if (_liveStreamManager == null) {
      return;
    }
    _liveCoreController?.unregisterConnectionObserver(_liveStreamManager!.liveStreamObserver);
    _liveCoreController?.unregisterBattleObserver(_liveStreamManager!.battleManagerObserver);
    _liveStreamManager!.floatWindowState.floatWindowMode.removeListener(_onFloatWindowModeChangedListener);
    widget.floatWindowController?.isFullScreen.removeListener(_onFullScreenChangedListener);
  }

  void _startWakeLock() {
    TUILiveKitPlatform.instance.enableWakeLock(true);
  }

  void _stopWakeLock() {
    TUILiveKitPlatform.instance.enableWakeLock(false);
  }

  void _onFloatWindowModeChanged() {
    if (_liveStreamManager == null) {
      return;
    }
    FloatWindowMode floatWindowMode = _liveStreamManager!.floatWindowState.floatWindowMode.value;
    if (floatWindowMode == FloatWindowMode.outOfApp) {
      widget.floatWindowController?.onSwitchFloatWindowOutOfApp.call(true);
    } else if (floatWindowMode == FloatWindowMode.none) {
      widget.floatWindowController?.onSwitchFloatWindowOutOfApp.call(false);
    }
    GlobalFloatWindowManager.instance.setFloatWindowMode(floatWindowMode);
  }

  void _onFullScreenChanged() {
    if (_liveStreamManager == null || widget.floatWindowController == null) {
      return;
    }
    bool isFullScreen = widget.floatWindowController!.isFullScreen.value;
    FloatWindowMode floatWindowMode = _liveStreamManager!.floatWindowState.floatWindowMode.value;
    if (isFullScreen) {
      if (floatWindowMode != FloatWindowMode.outOfApp) {
        _liveStreamManager?.setFloatWindowMode(FloatWindowMode.none);
      }
    } else {
      _liveStreamManager?.setFloatWindowMode(FloatWindowMode.inApp);
    }
  }
}
