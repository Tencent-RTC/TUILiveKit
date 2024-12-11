import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/state/index.dart';
import 'package:tencent_live_uikit/widget/live_room/anchor/index.dart';

class AnchorWidget extends BasicWidget {
  const AnchorWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AnchorWidgetState();
  }
}

class AnchorWidgetState extends BasicState<AnchorWidget> {

  @override
  void initState() {
    LiveKitLogger.info("AnchorWidget init");
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      resizeToAvoidBottomInset: false,
      body: Stack(
        children: [
          _initVideoWidget(),
          _initMaskWidget(),
          _initPreviewWidget(),
          _initLivingWidget(),
          _initDashboardWidget()
        ],
      ),
    );
  }

  @override
  void dispose() {
    LiveKitLogger.info("AnchorWidget dispose");
    liveController.roomController.exit();
    super.dispose();
  }

  _initVideoWidget() {
    return AnchorVideoWidget(liveController: liveController);
  }

  _initMaskWidget() {
    return Column(
      mainAxisAlignment: MainAxisAlignment.spaceBetween,
      children: [
        Container(
          decoration: const BoxDecoration(
            gradient: LinearGradient(
              begin: Alignment.topCenter,
              end: Alignment.bottomCenter,
              colors: [
                LivekitColors.livekitNotStandardBlack4DTransparency,
                LivekitColors.livekitNotStandardBlack00Transparency,
              ],
            ),
          ),
        ),
        Container(
          decoration: const BoxDecoration(
            gradient: LinearGradient(
              begin: Alignment.bottomCenter,
              end: Alignment.topCenter,
              colors: [
                LivekitColors.livekitNotStandardBlack4DTransparency,
                LivekitColors.livekitNotStandardBlack00Transparency,
              ],
            ),
          ),
        ),
      ],
    );
  }

  _initPreviewWidget() {
    return ValueListenableBuilder(
        valueListenable: liveController.getViewState().liveStatus,
        builder: (BuildContext context, LiveStatus value, Widget? child) {
          return Visibility(
              visible: liveController.getViewState().liveStatus.value == LiveStatus.previewing,
              child: AnchorPreviewWidget(liveController: liveController));
        });
  }

  _initLivingWidget() {
    return ValueListenableBuilder(
        valueListenable: liveController.getViewState().liveStatus,
        builder: (BuildContext context, LiveStatus value, Widget? child) {
          return Visibility(
              visible: liveController.getViewState().liveStatus.value != LiveStatus.previewing,
              child: AnchorLivingWidget(liveController: liveController));
        });
  }

  _initDashboardWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getViewState().liveStatus,
      builder: (BuildContext context, LiveStatus value, Widget? child) {
        return Visibility(
          visible: liveController.getViewState().liveStatus.value == LiveStatus.dashboard,
          child: AnchorDashboardWidget(liveController: liveController),
        );
      },
    );
  }
}
