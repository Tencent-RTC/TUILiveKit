import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:flutter/material.dart';

import '../../../../common/index.dart';
import '../../../../live_navigator_observer.dart';
import '../../../../live_stream/widget/live_room/audience/index.dart';
import '../../../state/index.dart';

class AudienceWidget extends BasicWidget {
  const AudienceWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AudienceWidgetState();
  }
}

class AudienceWidgetState extends BasicState {
  late final VoidCallback _listener = _onLiveStatusChange;

  @override
  void initState() {
    LiveKitLogger.info("AudienceWidget init");
    super.initState();
    _join();
    _addListener();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      resizeToAvoidBottomInset: false,
      body: ValueListenableBuilder(
        valueListenable: liveController.getViewState().liveStatus,
        builder: (BuildContext context, value, Widget? child) {
          return Stack(
            children: [
              _initAudienceVideoWidget(),
              _initAudienceLivingWidget(),
              _initDashboardWidget(),
            ],
          );
        },
      ),
    );
  }

  @override
  void dispose() {
    LiveKitLogger.info("AudienceWidget dispose");
    _removeListener();
    liveController.roomController.exit();
    BarrageDisplayController.resetState();
    GiftDisplayController.resetState();
    super.dispose();
  }

  Widget _initAudienceVideoWidget() {
    return AudienceVideoWidget(liveController: liveController);
  }

  Widget _initAudienceLivingWidget() {
    return AudienceLivingWidget(liveController: liveController);
  }

  Widget _initDashboardWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getViewState().liveStatus,
      builder: (BuildContext context, LiveStatus value, Widget? child) {
        return Visibility(
          visible: liveController.getViewState().liveStatus.value == LiveStatus.dashboard,
          child: AudienceDashboardWidget(liveController: liveController),
        );
      },
    );
  }
}

extension AudienceWidgetStateLogicExtension on AudienceWidgetState {
  void _join() {
    liveController.join();
  }

  void _addListener() {
    liveController.getViewState().liveStatus.addListener(_listener);
  }

  void _removeListener() {
    liveController.getViewState().liveStatus.removeListener(_listener);
  }

  void _onLiveStatusChange() {
    if (liveController.getViewState().liveStatus.value == LiveStatus.dashboard) {
      TUILiveKitNavigatorObserver.instance.backToLiveRoomAudiencePage();
    }
  }
}
