import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../../../../../common/index.dart';

class VideoParamsPanelWidget extends BasicWidget {
  const VideoParamsPanelWidget({super.key, required super.liveController});

  @override
  VideoParamsPanelWidgetState getState() {
    return VideoParamsPanelWidgetState();
  }
}

class VideoParamsPanelWidgetState extends BasicState<VideoParamsPanelWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      width: screenWidth,
      height: 350,
      decoration: const BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(topLeft: Radius.circular(20), topRight: Radius.circular(20)),
      ),
      child: Column(children: [
        _initTitleWidget(),
        _initResolutionWidget(),
      ]),
    );
  }

  _initTitleWidget() {
    return SizedBox(
      height: 44,
      width: screenWidth,
      child: Stack(
        children: [
          Positioned(
            left: 14,
            child: GestureDetector(
              onTap: () {
                Navigator.pop(context);
              },
              child: Container(
                width: 44,
                height: 44,
                padding: const EdgeInsets.all(10),
                child: Image.asset(
                  LiveImages.returnArrow,
                  package: Constants.pluginName,
                ),
              ),
            ),
          ),
          Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!.live_video_params,
              style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  _initResolutionWidget() {
    return Container(
      width: screenWidth,
      height: 50,
      padding: const EdgeInsets.only(left: 24, top: 20, right: 24),
      child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          crossAxisAlignment: CrossAxisAlignment.center,
          children: [
            Text(
              LiveKitLocalizations.of(Global.appContext())!.live_clarity,
              style: const TextStyle(color: LiveColors.designStandardG5, fontSize: 16),
            ),
            GestureDetector(
              onTap: () {
                _showResolutionSelectWidget();
              },
              child: SizedBox(
                width: 150,
                child: Row(
                  mainAxisAlignment: MainAxisAlignment.end,
                  children: [
                    ValueListenableBuilder(
                      valueListenable: liveController.getMediaState().videoQuality,
                      builder: (BuildContext context, TUIVideoQuality value, Widget? child) {
                        return Text(
                          _getResolutionShowText(),
                          style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 16),
                        );
                      },
                    ),
                    8.horizontalSpace,
                    Image.asset(
                      width: 20,
                      height: 20,
                      LiveImages.downArrow,
                      package: Constants.pluginName,
                    )
                  ],
                ),
              ),
            ),
          ]),
    );
  }
}

extension VideoParamsPanelWidgetStateLogicExtension on VideoParamsPanelWidgetState {
  String _getResolutionShowText() {
    switch (liveController.getMediaState().videoQuality.value) {
      case TUIVideoQuality.videoQuality_360P:
        return LiveKitLocalizations.of(Global.appContext())!.live_resolution_360p;
      case TUIVideoQuality.videoQuality_540P:
        return LiveKitLocalizations.of(Global.appContext())!.live_resolution_540p;
      case TUIVideoQuality.videoQuality_720P:
        return LiveKitLocalizations.of(Global.appContext())!.live_resolution_720p;
      case TUIVideoQuality.videoQuality_1080P:
        return LiveKitLocalizations.of(Global.appContext())!.live_resolution_1080p;
    }
  }

  void _showResolutionSelectWidget() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true, text: LiveKitLocalizations.of(Global.appContext())!.live_resolution_360p, bingData: 0),
      ActionSheetModel(
          isCenter: true, text: LiveKitLocalizations.of(Global.appContext())!.live_resolution_540p, bingData: 1),
      ActionSheetModel(
          isCenter: true, text: LiveKitLocalizations.of(Global.appContext())!.live_resolution_720p, bingData: 2),
      ActionSheetModel(
          isCenter: true, text: LiveKitLocalizations.of(Global.appContext())!.live_resolution_1080p, bingData: 3),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      liveController.mediaController.updateVideoQuality(_getResolutionByPosition(model.bingData));
    });
  }

  TUIVideoQuality _getResolutionByPosition(int position) {
    switch (position) {
      case 0:
        return TUIVideoQuality.videoQuality_360P;
      case 1:
        return TUIVideoQuality.videoQuality_540P;
      case 2:
        return TUIVideoQuality.videoQuality_720P;
      default:
        return TUIVideoQuality.videoQuality_1080P;
    }
  }
}
