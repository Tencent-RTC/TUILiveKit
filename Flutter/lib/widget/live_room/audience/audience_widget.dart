import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/state/index.dart';
import 'package:tencent_live_uikit/widget/live_navigator_observer.dart';
import 'package:tencent_live_uikit/widget/live_room/audience/component/live_streaming/audience_living_widget.dart';
import 'package:tencent_live_uikit/widget/live_room/audience/component/video/audience_video_widget.dart';

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
    _removeListener();
    liveController.roomController.exit();
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
