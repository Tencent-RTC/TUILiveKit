import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_prepare/widgets/anchor_preview_video_setting_panel_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_prepare/widgets/seat_layout_template_widget.dart';

import '../../../../common/constants/constants.dart';
import '../../../../common/language/index.dart';
import '../../../../common/resources/index.dart';
import '../../../../common/widget/index.dart';
import '../../../../component/audio_effect/index.dart';
import '../../../../component/beauty/index.dart';
import '../../../manager/live_stream_manager.dart';
import '../anchor_preview_widget_define.dart';

class AnchorPreviewFunctionWidget extends StatefulWidget {
  final EditInfo editInfo;
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;

  const AnchorPreviewFunctionWidget(
      {super.key, required this.editInfo, required this.liveStreamManager, required this.liveCoreController});

  @override
  State<AnchorPreviewFunctionWidget> createState() => _AnchorPreviewFunctionWidgetState();
}

class _AnchorPreviewFunctionWidgetState extends State<AnchorPreviewFunctionWidget> {
  late final LiveStreamManager liveStreamManager;
  late final LiveCoreController liveCoreController;

  @override
  void initState() {
    super.initState();
    liveStreamManager = widget.liveStreamManager;
    liveCoreController = widget.liveCoreController;
  }

  @override
  Widget build(BuildContext context) {
    return Row(mainAxisAlignment: MainAxisAlignment.spaceEvenly, children: [
      _buildBeautyWidget(),
      _buildAudioEffectWidget(),
      _buildCameraFlipWidget(),
      _buildTemplateWidget(),
      _buildVideoSettingWidget()
    ]);
  }

  Widget _buildBeautyWidget() {
    return GestureDetector(
      onTap: () {
        _showBeautyPanel();
      },
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: 36.radius,
            height: 36.radius,
            child: Image.asset(
              LiveImages.previewFunctionItemBeauty,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.common_video_settings_item_beauty,
            style: const TextStyle(
              fontSize: 12,
              color: LiveColors.designStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }

  Widget _buildAudioEffectWidget() {
    return GestureDetector(
      onTap: () {
        _showAudioEffectPanel();
      },
      child: Column(
        mainAxisSize: MainAxisSize.min,
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: 36.radius,
            height: 36.radius,
            child: Image.asset(
              LiveImages.prepareAudio,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.common_audio_effect,
            style: const TextStyle(
              fontSize: 12,
              color: LiveColors.designStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }

  Widget _buildCameraFlipWidget() {
    return GestureDetector(
      onTap: () {
        _clickCameraFlip();
      },
      child: Column(
        mainAxisSize: MainAxisSize.min,
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: 36.radius,
            height: 36.radius,
            child: Image.asset(
              LiveImages.previewFunctionItemFlip,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.common_video_settings_item_flip,
            style: const TextStyle(
              fontSize: 12,
              color: LiveColors.designStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }

  Widget _buildTemplateWidget() {
    return GestureDetector(
      onTap: () {
        _clickTemplate();
      },
      child: Column(
        mainAxisSize: MainAxisSize.min,
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: 36.radius,
            height: 36.radius,
            child: Image.asset(
              LiveImages.previewFunctionItemTemplate,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(context)!.common_template_layout,
            style: const TextStyle(
              fontSize: 12,
              color: LiveColors.designStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }

  Widget _buildVideoSettingWidget() {
    return GestureDetector(
      onTap: () {
        _clickVideoSettings();
      },
      child: Column(
        mainAxisSize: MainAxisSize.min,
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: 36.radius,
            height: 36.radius,
            child: Image.asset(
              LiveImages.previewFunctionItemSetting,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(context)!.common_video_settings,
            style: const TextStyle(
              fontSize: 12,
              color: LiveColors.designStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }
}

extension on _AnchorPreviewFunctionWidgetState {
  void _showBeautyPanel() async {
    popupWidget(const BeautyPanelWidget());
  }

  void _showAudioEffectPanel() {
    popupWidget(AudioEffectPanelWidget(roomId: liveStreamManager.roomState.roomId));
  }

  void _clickCameraFlip() {
    liveCoreController.switchCamera(!liveCoreController.mediaState.isFrontCamera);
  }

  void _clickTemplate() {
    popupWidget(SeatLayoutTemplateWidget(editInfo: widget.editInfo), backgroundColor: Color(0xFF131417));
  }

  void _clickVideoSettings() {
    popupWidget(AnchorPreviewVideoSettingPanelWidget(
      liveCoreController: liveCoreController,
      liveStreamManager: liveStreamManager,
    ));
  }
}
