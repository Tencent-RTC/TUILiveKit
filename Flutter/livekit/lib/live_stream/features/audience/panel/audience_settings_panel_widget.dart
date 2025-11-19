import 'package:flutter/material.dart';
import 'package:live_stream_core/common/logger/logger.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';

import '../../../../component/float_window/global_float_window_manager.dart';
import '../../../../component/float_window/pip_config_panel_widget.dart';

class AudienceSettingsPanelWidget extends StatelessWidget {
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;

  const AudienceSettingsPanelWidget({super.key, required this.liveStreamManager, required this.liveCoreController});

  @override
  Widget build(BuildContext context) {
    return Container(
        width: 1.screenWidth,
        height: 198.height,
        color: LiveColors.designBgColorOperate,
        child: Column(
          children: [
            SizedBox(height: 20.height),
            Center(
                child: Text(
              LiveKitLocalizations.of(context)!.common_more_features,
              style: TextStyle(
                  color: LiveColors.designStandardFlowkitWhite.withAlpha(230),
                  fontSize: 16,
                  fontWeight: FontWeight.w500),
            )),
            SizedBox(height: 20.height),
            Container(
              padding: EdgeInsets.symmetric(horizontal: 24.width),
              child: Row(
                children: [
                  _buildVideoSettingsItemWidget(context),
                  SizedBox(width: 12.width),
                  GlobalFloatWindowManager.instance.isEnableFloatWindowFeature()
                      ? _buildPipItemWidget(context)
                      : const SizedBox.shrink()
                ],
              ),
            )
          ],
        ));
  }

  Widget _buildVideoSettingsItemWidget(BuildContext context) {
    return _buildItemWidget(context, LiveKitLocalizations.of(context)!.live_video_resolution,
        LiveImages.videoResolution, () => _showVideoQualitySelectionPanel());
  }

  Widget _buildPipItemWidget(BuildContext context) {
    return _buildItemWidget(context, LiveKitLocalizations.of(context)!.common_video_settings_item_pip,
        LiveImages.settingsItemPip, () => _showPipConfigPanel());
  }

  Widget _buildItemWidget(BuildContext context, String title, String imageName, GestureTapCallback onTap) {
    return SizedBox(
      height: 80.height,
      width: 56.width,
      child: Column(children: [
        GestureDetector(
          onTap: onTap,
          child: Container(
              decoration: BoxDecoration(
                  color: LiveColors.designBgColorInput, borderRadius: BorderRadius.all(Radius.circular(8.radius))),
              width: 56.radius,
              height: 56.radius,
              child: Center(
                child: SizedBox(
                  width: 27.radius,
                  height: 27.radius,
                  child: Image.asset(
                    imageName,
                    package: Constants.pluginName,
                  ),
                ),
              )),
        ),
        SizedBox(height: 6.height),
        Center(
            child: Text(title,
                style: TextStyle(
                    color: LiveColors.designStandardFlowkitWhite.withAlpha(230),
                    fontSize: 12,
                    fontWeight: FontWeight.w400)))
      ]),
    );
  }
}

extension on AudienceSettingsPanelWidget {
  void _showVideoQualitySelectionPanel() {
    final List<ActionSheetModel> menuData = List.empty(growable: true);

    const lineColor = LiveColors.designBgColorInput;
    final textColor = LiveColors.designStandardFlowkitWhite.withAlpha(230);
    final playbackQualityList = liveStreamManager.mediaState.playbackQualityList.value;
    final Map<int, TUIVideoQuality> videoQualityMap = {};
    for (int i = 0; i < playbackQualityList.length; i++) {
      final videoQualityItemNumber = i + 1;
      final videoQuality = playbackQualityList[i];
      videoQualityMap[videoQualityItemNumber] = videoQuality;
      final videoQualityItem = ActionSheetModel(
          isCenter: true,
          text: _getVideoQualityString(videoQuality),
          textStyle: TextStyle(color: textColor, fontSize: 16),
          lineColor: lineColor,
          bingData: videoQualityItemNumber);
      menuData.add(videoQualityItem);
    }
    final cancelNumber = playbackQualityList.length + 1;

    final cancel = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
        textStyle: TextStyle(color: textColor, fontSize: 16),
        lineColor: lineColor,
        bingData: cancelNumber);
    menuData.add(cancel);

    ActionSheet.show(menuData, (model) {
      if (!videoQualityMap.containsKey(model.bingData)) {
        return;
      }
      final TUIVideoQuality videoQuality = videoQualityMap[model.bingData]!;
      liveStreamManager.switchPlaybackQuality(videoQuality);
    }, backgroundColor: LiveColors.designBgColorOperate);
  }

  String _getVideoQualityString(TUIVideoQuality videoQuality) {
    switch (videoQuality) {
      case TUIVideoQuality.videoQuality_1080P:
        return '1080P';
      case TUIVideoQuality.videoQuality_720P:
        return '720P';
      case TUIVideoQuality.videoQuality_540P:
        return '540P';
      case TUIVideoQuality.videoQuality_360P:
        return '360P';
      default:
        return 'unknown';
    }
  }

  void _showPipConfigPanel() {
    TUILiveKitPlatform.instance.hasPipPermission().then((hasPipPermission) {
      if (!hasPipPermission) {
        liveStreamManager.enablePipMode(false);
      }
      popupWidget(
        PipConfigPanelWidget(
          enablePipMode: liveStreamManager.floatWindowState.enablePipMode,
          onChanged: (enable) {
            Navigator.pop(Global.appContext());
            _enablePictureInPicture(enable);
            if (enable && !hasPipPermission) {
              TUILiveKitPlatform.instance.openPipSettings();
            }
          },
        ),
      );
    });
  }

  void _enablePictureInPicture(bool enable) {
    if (GlobalFloatWindowManager.instance.isEnableFloatWindowFeature()) {
      final roomId = liveCoreController.roomState.roomId;
      final jsonString = liveStreamManager.buildEnablePipJsonParams(enable, roomId);
      liveCoreController.enablePictureInPicture(jsonString).then((result) {
        LiveStreamCoreLogger.info("enablePictureInPicture,enable=$enable,result=$result");
        liveStreamManager.enablePipMode(enable && result);
      });
    }
  }
}
