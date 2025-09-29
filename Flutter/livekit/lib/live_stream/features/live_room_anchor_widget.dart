import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:live_stream_core/live_core_widget/index.dart' hide LiveStatus;
import 'package:live_uikit_barrage/widget/display/barrage_display_controller.dart';
import 'package:live_uikit_gift/widget/gift/display/gift_play_controller.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/error/error_handler.dart';
import 'package:tencent_live_uikit/component/beauty/state/beautyStateFactory.dart';

import '../../common/language/index.dart';
import '../../common/logger/index.dart';
import '../../common/platform/index.dart';
import '../../common/reporter/live_data_reporter.dart';
import '../../common/widget/toast.dart';
import '../../component/audio_effect/index.dart';
import '../../component/beauty/index.dart';
import '../../live_navigator_observer.dart';
import '../live_define.dart';
import '../manager/live_stream_manager.dart';
import 'anchor_broadcast/index.dart';
import 'anchor_prepare/anchor_preview_widget.dart';

class TUILiveRoomAnchorWidget extends StatefulWidget {
  final String roomId;
  final bool needPrepare;
  final TUILiveInfo? liveInfo;
  final void Function()? onStartLive;

  const TUILiveRoomAnchorWidget(
      {super.key, required this.roomId, this.needPrepare = true, this.liveInfo, this.onStartLive});

  @override
  State<TUILiveRoomAnchorWidget> createState() => _TUILiveRoomAnchorWidgetState();
}

class _TUILiveRoomAnchorWidgetState extends State<TUILiveRoomAnchorWidget> {
  late final LiveCoreController _liveCoreController;
  late final LiveStreamManager _liveStreamManager;
  final ValueNotifier<bool> _isShowingPreviewWidget = ValueNotifier(false);

  @override
  void initState() {
    super.initState();
    LiveDataReporter.reportComponent(LiveComponentType.liveRoom);
    _changeStatusBar2LightMode();
    _initLiveStream();
    _addObserver();
    _startForegroundService();
    _startWakeLock();
  }

  @override
  void dispose() {
    _stopWakeLock();
    _stopForegroundService();
    _removeObserver();
    _unInitLiveStream();
    AudioEffectStateFactory.removeState(widget.roomId);
    BeautyStateFactory.removeState(BeautyManager.beautyStateKey);
    BarrageDisplayController.resetState();
    GiftPlayController.resetState();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      resizeToAvoidBottomInset: false,
      body: Stack(children: [_buildAnchorBroadcastWidget(), _buildAnchorPreviewWidget()]),
    );
  }

  Widget _buildAnchorBroadcastWidget() {
    return ValueListenableBuilder(
        valueListenable: _isShowingPreviewWidget,
        builder: (context, showPreview, _) {
          return Visibility(
            visible: !showPreview,
            child:
                AnchorBroadcastWidget(liveStreamManager: _liveStreamManager, liveCoreController: _liveCoreController),
          );
        });
  }

  Widget _buildAnchorPreviewWidget() {
    return ValueListenableBuilder(
        valueListenable: _isShowingPreviewWidget,
        builder: (context, showPreview, _) {
          return Visibility(
            visible: showPreview,
            child: AnchorPreviewWidget(
              liveStreamManager: _liveStreamManager,
              liveCoreController: _liveCoreController,
              didClickBack: () {
                Navigator.of(context).pop();
              },
              didClickStart: (editInfo) {
                _liveStreamManager.coHostManager.setLayoutTemplateId(editInfo.coHostTemplateMode.value.id);
                _startLiveStream(editInfo.roomName.value, editInfo.coverUrl.value, editInfo.privacyMode.value,
                    editInfo.coGuestTemplateMode.value);
              },
            ),
          );
        });
  }
}

extension on _TUILiveRoomAnchorWidgetState {
  void _changeStatusBar2LightMode() {
    SystemChrome.setSystemUIOverlayStyle(SystemUiOverlayStyle.light);
  }

  void _initLiveStream() async {
    _liveCoreController = LiveCoreController();
    _liveStreamManager = LiveStreamManager(provider: CoreStateProvider(getCoreState: _liveCoreController.getCoreState));

    widget.liveInfo != null
        ? _liveStreamManager.prepareLiveInfoBeforeEnterRoom(widget.liveInfo!)
        : _liveStreamManager.prepareRoomIdBeforeEnterRoom(widget.roomId);
    final isObsBroadcast = await _checkWhetherObsBroadcast(widget.roomId);

    widget.needPrepare && !isObsBroadcast ? _liveStreamManager.onStartPreview() : _joinSelfCreatedRoom(isObsBroadcast);

    _isShowingPreviewWidget.value = !isObsBroadcast && widget.needPrepare;
  }

  void _unInitLiveStream() {
    _liveCoreController.dispose();
    _liveStreamManager.dispose();
  }

  void _joinSelfCreatedRoom(bool isObsBroadcast) async {
    if (!isObsBroadcast && !_liveCoreController.mediaState.isCameraOpened.value) {
      _startCameraAndMicrophone();
    }

    final result = await _liveCoreController.joinLiveStreamV2(widget.roomId);
    if (result.code != TUIError.success || result.data == null) {
      _toastAndPop(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      return;
    }

    widget.onStartLive?.call();
    _liveStreamManager.onStartLive(true, result.data!);
  }

  void _startLiveStream(
      String? roomName, String? coverUrl, LiveStreamPrivacyStatus? privacyMode, LiveTemplateMode templateMode) async {
    _isShowingPreviewWidget.value = false;
    widget.onStartLive?.call();

    if (roomName != null) {
      _liveStreamManager.onSetRoomName(roomName);
    }
    if (coverUrl != null) {
      _liveStreamManager.onSetRoomCoverUrl(coverUrl);
    }
    if (privacyMode != null) {
      _liveStreamManager.onSetRoomPrivacy(privacyMode);
    }

    final liveInfo = TUILiveInfo();
    liveInfo.roomId = widget.roomId;
    liveInfo.name = _liveStreamManager.roomState.roomName;
    liveInfo.isSeatEnabled = true;
    liveInfo.seatMode = TUISeatMode.applyToTake;
    liveInfo.coverUrl = coverUrl ?? "";
    liveInfo.backgroundUrl = coverUrl ?? "";
    liveInfo.isPublicVisible = privacyMode == LiveStreamPrivacyStatus.public;
    liveInfo.activityStatus = widget.liveInfo?.activityStatus ?? 0;
    liveInfo.keepOwnerOnSeat = true;
    liveInfo.seatLayoutTemplateId = templateMode.id;

    final result = await _liveCoreController.startLiveStreamV2(liveInfo);
    if (result.code != TUIError.success || result.data == null) {
      _toastAndPop(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      return;
    }

    _liveStreamManager.onStartLive(false, result.data!);
  }

  Future<bool> _checkWhetherObsBroadcast(String roomId) async {
    final result = await _liveStreamManager.fetchLiveInfo(roomId);
    if (result.code != TUIError.success || result.data == null) {
      return false;
    }
    final TUILiveInfo liveInfo = result.data!;
    return !liveInfo.keepOwnerOnSeat;
  }

  void _toastAndPop(String toast) {
    makeToast(msg: toast);
    Future.delayed(const Duration(seconds: 2), () {
      if (mounted) {
        Navigator.of(context).pop();
      }
    });
  }

  void _startCameraAndMicrophone() async {
    final startCameraResult = await _liveCoreController.startCamera(true);
    if (startCameraResult.code != TUIError.success) {
      _liveStreamManager.toastSubject
          .add(ErrorHandler.convertToErrorMessage(startCameraResult.code.rawValue, startCameraResult.message) ?? '');
    }
    final startMicrophoneResult = await _liveCoreController.startMicrophone();
    if (startMicrophoneResult.code != TUIError.success) {
      _liveStreamManager.toastSubject.add(
          ErrorHandler.convertToErrorMessage(startMicrophoneResult.code.rawValue, startMicrophoneResult.message) ?? '');
    }
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
    _liveCoreController.registerConnectionObserver(_liveStreamManager.liveStreamObserver);
    _liveCoreController.registerBattleObserver(_liveStreamManager.battleManagerObserver);
  }

  void _removeObserver() {
    _liveCoreController.unregisterConnectionObserver(_liveStreamManager.liveStreamObserver);
    _liveCoreController.unregisterBattleObserver(_liveStreamManager.battleManagerObserver);
  }

  void _startWakeLock() async {
    TUILiveKitPlatform.instance.enableWakeLock(true);
  }

  void _stopWakeLock() async {
    TUILiveKitPlatform.instance.enableWakeLock(false);
  }
}
