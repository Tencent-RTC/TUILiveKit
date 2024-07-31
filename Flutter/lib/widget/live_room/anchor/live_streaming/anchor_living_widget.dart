import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/manager/index.dart';
import 'package:tencent_live_uikit/state/index.dart';
import 'package:tencent_live_uikit/widget/live_room/anchor/live_streaming/anchor_living_function_widget.dart';

import 'link/apply_link_mic_float_widget.dart';

class AnchorLivingWidget extends BasicWidget {
  const AnchorLivingWidget({super.key, required super.liveController});

  @override
  AnchorLivingWidgetState getState() {
    return AnchorLivingWidgetState();
  }
}

class AnchorLivingWidgetState extends BasicState<AnchorLivingWidget> {
  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        _initBackWidget(),
        _initLiveInfoWidget(),
        _initAudienceListWidget(),
        _initFunctionWidget(),
        _initApplyLinkAudienceWidget(),
        _initBarrageWidget()
      ],
    );
  }

  _initBackWidget() {
    return Positioned(
      right: 16,
      top: 58,
      width: 24,
      height: 24,
      child: GestureDetector(
        onTap: () {
          _closeLiveRoom();
        },
        child: Image.asset(
          LivekitImages.livekitClose,
          package: Constants.pluginName,
        ),
      ),
    );
  }

  _initLiveInfoWidget() {
    return Positioned(
        left: 16,
        top: 54,
        height: 32,
        width: 170,
        child: LiveInfoWidget(
          liveController: liveController,
        ));
  }

  _initAudienceListWidget() {
    return Positioned(
        right: 48,
        top: 58,
        width: 122,
        height: 24,
        child: AudienceListWidget(
          liveController: liveController,
        ));
  }

  _initFunctionWidget() {
    return Positioned(
      left: 0,
      bottom: 34,
      height: 36,
      width: MediaQuery.of(Global.appContext()).size.width,
      child: AnchorLivingFunctionWidget(
        liveController: liveController,
      ),
    );
  }

  _initBarrageWidget() {
    return Positioned(
        left: 16,
        bottom: 80,
        height: 212,
        width: MediaQuery.of(Global.appContext()).size.width - 72,
        child: Container(
          color: Colors.transparent,
        ));
  }

  _initApplyLinkAudienceWidget() {
    return Positioned(
      right: 8,
      top: 90,
      height: 86,
      width: 114,
      child: ApplyLinkMicFloatWidget(
        liveController: liveController,
      ),
    );
  }
}

extension AnchorLivingWidgetStateLogicExtension on AnchorLivingWidgetState {
  _closeLiveRoom() {
    liveController.viewController.updateLiveStatus(LiveStatus.dashboard);
    RoomController roomController = liveController.roomController;
    roomController.updateLikeNumber(0);
    roomController.updateMessageCount(0);
    roomController.exit();
  }
}
