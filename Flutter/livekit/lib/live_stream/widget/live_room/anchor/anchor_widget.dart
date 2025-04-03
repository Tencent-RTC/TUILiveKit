import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:flutter/material.dart';

import '../../../../common/index.dart';
import '../../../state/index.dart';
import 'index.dart';

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
    BarrageDisplayController.resetState();
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
                LiveColors.notStandardBlack4DTransparency,
                LiveColors.notStandardBlack00Transparency,
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
                LiveColors.notStandardBlack4DTransparency,
                LiveColors.notStandardBlack00Transparency,
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
