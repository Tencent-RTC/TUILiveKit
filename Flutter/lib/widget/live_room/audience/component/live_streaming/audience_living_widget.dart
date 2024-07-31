import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/widget/live_room/audience/component/live_streaming/audience_function_widget.dart';
import 'package:tencent_live_uikit/widget/live_room/audience/component/live_streaming/audience_waiting_pass_widget.dart';

class AudienceLivingWidget extends BasicWidget {
  const AudienceLivingWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AudienceLivingWidgetState();
  }
}

class AudienceLivingWidgetState extends BasicState<AudienceLivingWidget> {
  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: liveController.getViewState().liveStatus,
      builder: (BuildContext context, value, Widget? child) {
        return Stack(
          children: [
            _initLiveInfoWidget(),
            _initAudienceListWidget(),
            _initCloseWidget(),
            _initAudienceWaitingPassWidget(),
            _initBarrageWidget(),
            _initFunctionWidget()
          ],
        );
      },
    );
  }

  Widget _initLiveInfoWidget() {
    return Positioned(
        left: 16,
        top: 54,
        height: 32,
        width: 170,
        child: LiveInfoWidget(
          liveController: liveController,
        ));
  }

  Widget _initAudienceListWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getUserState().userList,
      builder: (BuildContext context, value, Widget? child) {
        return Stack(
          children: [
            Positioned(
              top: 57,
              right: 48,
              child: AudienceListWidget(liveController: liveController),
            ),
          ],
        );
      },
    );
  }

  Widget _initCloseWidget() {
    return Positioned(
        top: 57,
        right: 14,
        width: 24,
        height: 24,
        child: GestureDetector(
          onTap: () {
            liveController.exit();
            Navigator.pop(context);
          },
          child: Image.asset(
            LivekitImages.livekitAudienceClose,
            package: Constants.pluginName,
          ),
        ));
  }

  Widget _initAudienceWaitingPassWidget() {
    return Positioned(right: 8, top: 90, child: AudienceWaitingPassWidget(liveController: liveController));
  }

  Widget _initBarrageWidget() {
    return Positioned(
        left: 16,
        bottom: 80,
        height: 212,
        width: screenWidth - 72,
        child: Container(
          color: Colors.transparent,
        ));
  }

  Widget _initFunctionWidget() {
    return Positioned(
      left: 0,
      bottom: 34,
      height: 36,
      width: screenWidth,
      child: AudienceFunctionWidget(
        liveController: liveController,
      ),
    );
  }
}
