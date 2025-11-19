import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/component/index.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

import '../../../manager/live_stream_manager.dart';

class AnchorPreviewVideoSettingPanelWidget extends StatefulWidget {
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;

  const AnchorPreviewVideoSettingPanelWidget(
      {super.key, required this.liveCoreController, required this.liveStreamManager});

  @override
  State<AnchorPreviewVideoSettingPanelWidget> createState() => _AnchorPreviewVideoSettingPanelWidgetState();
}

class _AnchorPreviewVideoSettingPanelWidgetState extends State<AnchorPreviewVideoSettingPanelWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      height: 252.height,
      width: 1.screenWidth,
      decoration: const BoxDecoration(color: LiveColors.designBgColorOperate),
      child: Column(
        mainAxisSize: MainAxisSize.min,
        children: [
          SizedBox(height: 20.height),
          Center(
              child: Text(LiveKitLocalizations.of(context)!.common_video_settings,
                  style: TextStyle(
                      color: LiveColors.designStandardFlowkitWhite.withAlpha(230),
                      fontSize: 16,
                      fontWeight: FontWeight.w500))),
          SizedBox(height: 20.height),
          _buildVideoSettingsItemWidget()
        ],
      ),
    );
  }

  Widget _buildVideoSettingsItemWidget() {
    return Container(
      margin: EdgeInsets.symmetric(horizontal: 16.width),
      decoration: BoxDecoration(
          color: LiveColors.designBgColorInput, borderRadius: BorderRadius.all(Radius.circular(8.radius))),
      height: 113.height,
      child: Column(
        children: [
          Container(
            height: 56.height,
            padding: EdgeInsets.only(left: 12.width, right: 14.width, top: 18.height, bottom: 18.height),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                Text(LiveKitLocalizations.of(context)!.common_video_settings_item_mirror,
                    style: TextStyle(
                        color: LiveColors.designStandardFlowkitWhite.withAlpha(230),
                        fontSize: 16,
                        fontWeight: FontWeight.w400)),
                ValueListenableBuilder(
                    valueListenable: widget.liveCoreController.mediaState.isMirrorEnable,
                    builder: (context, isMirror, _) {
                      return Switch(
                          value: widget.liveCoreController.mediaState.isMirrorEnable.value,
                          activeTrackColor: Colors.blue,
                          onChanged: (value) {
                            if (value == widget.liveCoreController.mediaState.isMirrorEnable.value) {
                              return;
                            }
                            widget.liveCoreController.enableMirror(value);
                          });
                    }),
              ],
            ),
          ),
          Container(
              padding: EdgeInsets.symmetric(horizontal: 16.width),
              child: Container(height: 1.height, color: LiveColors.designStrokeColorPrimary)),
          Container(
            height: 56.height,
            padding: EdgeInsets.only(left: 12.width, right: 14.width, top: 18.height, bottom: 18.height),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                Text(LiveKitLocalizations.of(context)!.live_video_resolution,
                    style: TextStyle(
                        color: LiveColors.designStandardFlowkitWhite.withAlpha(230),
                        fontSize: 16,
                        fontWeight: FontWeight.w400)),
                GestureDetector(
                  onTap: () {
                    _showVideoQualitySelectionPanel();
                  },
                  child: ValueListenableBuilder(
                      valueListenable: widget.liveStreamManager.mediaState.videoQuality,
                      builder: (context, videoQuality, _) {
                        return Row(
                          spacing: 4.width,
                          children: [
                            Text(_getVideoQualityString(widget.liveStreamManager.mediaState.videoQuality.value),
                                style: TextStyle(
                                    color: LiveColors.designStandardFlowkitWhite.withAlpha(230),
                                    fontSize: 16,
                                    fontWeight: FontWeight.w400)),
                            Image.asset(LiveImages.downArrow, package: Constants.pluginName)
                          ],
                        );
                      }),
                )
              ],
            ),
          )
        ],
      ),
    );
  }
}

extension on _AnchorPreviewVideoSettingPanelWidgetState {
  void _showVideoQualitySelectionPanel() {
    const videoQuality1080PNumber = 1;
    const videoQuality720PNumber = 2;
    const cancelNumber = 3;
    final List<ActionSheetModel> menuData = List.empty(growable: true);

    const lineColor = LiveColors.designBgColorInput;
    final textColor = LiveColors.designStandardFlowkitWhite.withAlpha(230);

    final videoQuality1080P = ActionSheetModel(
        isCenter: true,
        text: _getVideoQualityString(TUIVideoQuality.videoQuality_1080P),
        textStyle: TextStyle(color: textColor, fontSize: 16),
        lineColor: lineColor,
        bingData: videoQuality1080PNumber);
    menuData.add(videoQuality1080P);

    final videoQuality720P = ActionSheetModel(
        isCenter: true,
        text: _getVideoQualityString(TUIVideoQuality.videoQuality_720P),
        textStyle: TextStyle(color: textColor, fontSize: 16),
        lineColor: lineColor,
        bingData: videoQuality720PNumber);
    menuData.add(videoQuality720P);

    final cancel = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
        textStyle: TextStyle(color: textColor, fontSize: 16),
        lineColor: lineColor,
        bingData: cancelNumber);
    menuData.add(cancel);

    ActionSheet.show(menuData, (model) {
      switch (model.bingData) {
        case videoQuality1080PNumber:
          widget.liveStreamManager.updateVideoQuality(TUIVideoQuality.videoQuality_1080P);
          break;
        case videoQuality720PNumber:
          widget.liveStreamManager.updateVideoQuality(TUIVideoQuality.videoQuality_720P);
          break;
        default:
          break;
      }
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
}
