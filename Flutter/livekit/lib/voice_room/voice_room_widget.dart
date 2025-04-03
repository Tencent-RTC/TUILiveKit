import 'dart:async';

import 'package:flutter/material.dart';
import 'package:live_stream_core/live_stream_core.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:permission_handler/permission_handler.dart';

import '../common/language/index.dart';
import '../common/platform/index.dart';
import '../common/widget/index.dart';
import '../live_navigator_observer.dart';
import './index.dart';
import 'widget/component/audio_effect/index.dart';

const maxConnectedViewersCount = 10;

enum RoomBehavior { autoCreate, prepareCreate, join }

class RoomParams {
  int maxSeatCount;
  TUISeatMode seatMode;

  RoomParams(
      {this.maxSeatCount = maxConnectedViewersCount,
      this.seatMode = TUISeatMode.applyToTake});
}

class TUIVoiceRoomWidget extends StatefulWidget {
  final String roomId;
  final RoomBehavior behavior;
  final RoomParams? params;

  const TUIVoiceRoomWidget(
      {super.key, required this.roomId, required this.behavior, this.params});

  @override
  State<TUIVoiceRoomWidget> createState() => _TUIVoiceRoomWidgetState();
}

class _TUIVoiceRoomWidgetState extends State<TUIVoiceRoomWidget> {
  late final String roomId;
  late final RoomBehavior behavior;
  late final RoomParams? params;
  late final VoiceRoomManager manager;
  late final SeatGridController seatGridController;

  late final ValueNotifier<bool> _needToPrepare = ValueNotifier(false);
  late final StreamSubscription<String> _toastSubscription;

  @override
  void initState() {
    super.initState();
    _startForegroundService();
    roomId = widget.roomId;
    behavior = widget.behavior;
    params = widget.params;
    manager = VoiceRoomManager();
    seatGridController = SeatGridController();

    _subscribeToast();
    manager.initManager(roomId: roomId, param: params);

    _needToPrepare.value = behavior != RoomBehavior.join;
  }

  @override
  void dispose() {
    _unsubscribeToast();
    manager.dispose();
    AudioEffectStateFactory.removeState(manager.roomState.roomId);
    _stopForegroundService();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      resizeToAvoidBottomInset: false,
      body: Stack(
        children: [_initVoiceRoomRootWidget(), _initVoiceRoomPrepareWidget()],
      ),
    );
  }

  Widget _initVoiceRoomPrepareWidget() {
    return ValueListenableBuilder(
        valueListenable: _needToPrepare,
        builder: (context, value, child) {
          return Visibility(
              visible: value,
              child: VoiceRoomPrepareWidget(
                  manager: manager,
                  didClickStart: () {
                    _needToPrepare.value = false;
                  }));
        });
  }

  Widget _initVoiceRoomRootWidget() {
    return ValueListenableBuilder(
        valueListenable: _needToPrepare,
        builder: (context, value, child) {
          return Visibility(
              visible: !value,
              child: VoiceRoomRootWidget(
                  roomId: roomId,
                  manager: manager,
                  seatGridController: seatGridController,
                  isCreate: behavior != RoomBehavior.join));
        });
  }
}

extension on _TUIVoiceRoomWidgetState {
  void _subscribeToast() {
    _toastSubscription = manager.toastSubject.stream.listen((toast) {
      makeToast(msg: toast);
    });
  }

  void _unsubscribeToast() {
    _toastSubscription.cancel();
  }

  void _startForegroundService() async {
    String description = LiveKitLocalizations.of(
            TUILiveKitNavigatorObserver.instance.getContext())!
        .live_app_running;

    Permission.microphone.onGrantedCallback(() {
      TUILiveKitPlatform.instance
          .startForegroundService(ForegroundServiceType.audio, "", description);
    });
  }

  void _stopForegroundService() {
    TUILiveKitPlatform.instance
        .stopForegroundService(ForegroundServiceType.audio);
    Permission.microphone.onGrantedCallback(null);
  }
}
