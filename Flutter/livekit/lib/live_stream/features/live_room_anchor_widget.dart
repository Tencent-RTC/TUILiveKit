import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:live_stream_core/live_core_widget/index.dart' hide LiveStatus;
import 'package:permission_handler/permission_handler.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/error/error_handler.dart';
import 'package:tencent_live_uikit/component/beauty/state/beautyStateFactory.dart';

import '../../common/language/index.dart';
import '../../common/platform/index.dart';
import '../../common/reporter/live_data_reporter.dart';
import '../../component/audio_effect/index.dart';
import '../../component/beauty/index.dart';
import '../../live_navigator_observer.dart';
import '../manager/live_stream_manager.dart';
import '../live_define.dart';
import 'anchor_broadcast/index.dart';
import 'anchor_prepare/anchor_preview_widget.dart';

class TUILiveRoomAnchorWidget extends StatefulWidget {
  final String roomId;
  final bool needPrepare;
  final TUILiveInfo? liveInfo;
  final void Function()? onStartLive;

  const TUILiveRoomAnchorWidget(
      {super.key,
      required this.roomId,
      this.needPrepare = true,
      this.liveInfo,
      this.onStartLive});

  @override
  State<TUILiveRoomAnchorWidget> createState() =>
      _TUILiveRoomAnchorWidgetState();
}

class _TUILiveRoomAnchorWidgetState extends State<TUILiveRoomAnchorWidget> {
  late final LiveCoreController _liveCoreController;
  late final LiveStreamManager _liveStreamManager;
  final ValueNotifier<bool> _isShowingPreviewWidget = ValueNotifier(true);

  @override
  void initState() {
    super.initState();
    LiveDataReporter.reportComponent(LiveComponentType.liveRoom);
    _changeStatusBar2LightMode();
    _initLiveStream();
    _addObserver();
    _startForegroundService();
  }

  @override
  void dispose() {
    _stopForegroundService();
    _removeObserver();
    _unInitLiveStream();
    AudioEffectStateFactory.removeState(widget.roomId);
    BeautyStateFactory.removeState(BeautyManager.beautyStateKey);
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Stack(children: [
        _buildAnchorBroadcastWidget(),
        _buildAnchorPreviewWidget()
      ]),
    );
  }

  Widget _buildAnchorBroadcastWidget() {
    return ValueListenableBuilder(
        valueListenable: _isShowingPreviewWidget,
        builder: (context, showPreview, _) {
          return Visibility(
            visible: !showPreview,
            child: AnchorBroadcastWidget(
                liveStreamManager: _liveStreamManager,
                liveCoreController: _liveCoreController),
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
                _startLiveStream(editInfo.roomName.value,
                    editInfo.coverUrl.value, editInfo.privacyMode.value);
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

  void _initLiveStream() {
    _liveCoreController = LiveCoreController();
    _liveStreamManager = LiveStreamManager(
        provider:
            CoreStateProvider(getCoreState: _liveCoreController.getCoreState));

    widget.liveInfo != null
        ? _liveStreamManager.prepareLiveInfoBeforeEnterRoom(widget.liveInfo!)
        : _liveStreamManager.prepareRoomIdBeforeEnterRoom(widget.roomId);

    widget.needPrepare
        ? _liveStreamManager.onStartPreview()
        : _joinSelfCreatedRoom();

    _isShowingPreviewWidget.value = widget.needPrepare;
  }

  void _unInitLiveStream() {
    _liveCoreController.dispose();
    _liveStreamManager.dispose();
  }

  void _joinSelfCreatedRoom() async {
    final result = await _liveCoreController.joinLiveStream(widget.roomId);
    if (result.code != TUIError.success || result.data == null) {
      _toastAndPop(ErrorHandler.convertToErrorMessage(
              result.code.rawValue, result.message) ??
          '');
      return;
    }

    widget.onStartLive?.call();
    _liveStreamManager.onStartLive(true, result.data!);
  }

  void _startLiveStream(String? roomName, String? coverUrl,
      LiveStreamPrivacyStatus? privacyMode) async {
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

    final roomInfo = TUIRoomInfo(roomId: widget.roomId);
    roomInfo.name = _liveStreamManager.roomState.roomName;
    roomInfo.isSeatEnabled = true;
    roomInfo.roomType = TUIRoomType.livingRoom;
    roomInfo.seatMode = TUISeatMode.applyToTake;
    final result = await _liveCoreController.startLiveStream(roomInfo);
    if (result.code != TUIError.success || result.data == null) {
      _toastAndPop(ErrorHandler.convertToErrorMessage(
              result.code.rawValue, result.message) ??
          '');
      return;
    }

    _liveStreamManager.onStartLive(false, result.data!);
  }

  void _toastAndPop(String toast) {
    _liveStreamManager.toastSubject.add(toast);
    Future.delayed(const Duration(seconds: 2), () {
      if (mounted) {
        Navigator.of(context).pop();
      }
    });
  }

  void _startForegroundService() {
    String description = LiveKitLocalizations.of(
            TUILiveKitNavigatorObserver.instance.getContext())!
        .common_app_running;
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

  void _addObserver() {
    _liveCoreController
        .registerConnectionObserver(_liveStreamManager.liveStreamObserver);
    _liveCoreController
        .registerBattleObserver(_liveStreamManager.battleManagerObserver);
  }

  void _removeObserver() {
    _liveCoreController
        .unregisterConnectionObserver(_liveStreamManager.liveStreamObserver);
    _liveCoreController
        .unregisterBattleObserver(_liveStreamManager.battleManagerObserver);
  }
}
