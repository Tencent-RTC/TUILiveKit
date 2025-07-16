import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/constants/constants.dart';
import 'package:tencent_live_uikit/common/error/error_handler.dart';
import 'package:tencent_live_uikit/common/language/gen/livekit_localizations.dart';
import 'package:tencent_live_uikit/common/resources/colors.dart';
import 'package:tencent_live_uikit/common/resources/images.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';
import 'package:tencent_live_uikit/common/widget/index.dart';
import 'package:tencent_live_uikit/live_navigator_observer.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';

import '../../../../component/beauty/index.dart';

class CoGuestVideoSettingsPanelWidget extends StatefulWidget {
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;

  const CoGuestVideoSettingsPanelWidget({
    super.key,
    required this.liveCoreController,
    required this.liveStreamManager,
  });

  @override
  State<CoGuestVideoSettingsPanelWidget> createState() =>
      _CoGuestVideoSettingsPanelWidgetState();
}

class _CoGuestVideoSettingsPanelWidgetState
    extends State<CoGuestVideoSettingsPanelWidget> {
  @override
  void dispose() {
    widget.liveCoreController.stopCamera();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      width: 1.screenWidth,
      height: 718.height,
      decoration: _buildPanelDecoration(),
      child: Column(
        children: [
          SizedBox(height: 20.height),
          _buildTitleWidget(),
          SizedBox(height: 22.height),
          _buildCameraPreviewWidget(),
          SizedBox(height: 24.height),
          _buildVideoSettingWidgets(),
          SizedBox(height: 110.height),
          _buildApplyLinkMicWidget(),
          SizedBox(height: 20.height),
          _buildApplyLinkMicTipsWidget(),
        ],
      ),
    );
  }

  BoxDecoration _buildPanelDecoration() => BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(
          topLeft: Radius.circular(20.radius),
          topRight: Radius.circular(20.radius),
        ),
      );

  Widget _buildTitleWidget() => SizedBox(
        height: 44.height,
        child: Center(
          child: Text(
            LiveKitLocalizations.of(Global.appContext())!
                .common_title_link_video_settings,
            style: const TextStyle(
              color: LiveColors.designStandardG7,
              fontSize: 16,
            ),
          ),
        ),
      );

  Widget _buildCameraPreviewWidget() => ClipRRect(
        borderRadius: BorderRadius.all(Radius.circular(16.radius)),
        child: SizedBox(
          width: 328.width,
          height: 328.height,
          child: VideoView(
            onViewCreated: (id) {
              widget.liveCoreController.setLocalVideoView(id);
              widget.liveCoreController.startCamera(
                widget.liveCoreController.mediaState.isFrontCamera,
              );
            },
          ),
        ),
      );

  Widget _buildVideoSettingWidgets() => Row(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          _buildVideoSettingWidget(
            LiveImages.videoSettingsBeauty,
            LiveKitLocalizations.of(Global.appContext())!
                .common_video_settings_item_beauty,
            _showBeautyPanel,
          ),
          SizedBox(width: 12.width),
          _buildVideoSettingWidget(
            LiveImages.videoSettingsFlip,
            LiveKitLocalizations.of(Global.appContext())!
                .common_video_settings_item_flip,
            _switchCamera,
          ),
          SizedBox(width: 12.width),
        ],
      );

  Widget _buildVideoSettingWidget(
          String image, String title, VoidCallback onTap) =>
      GestureDetector(
        onTap: onTap,
        child: ClipRRect(
          borderRadius: BorderRadius.all(Radius.circular(10.radius)),
          child: Container(
            color: LiveColors.designStandardG3,
            width: 56.width,
            height: 56.height,
            child: Column(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                SizedBox(
                  width: 24.width,
                  height: 24.height,
                  child: Image.asset(
                    image,
                    package: Constants.pluginName,
                  ),
                ),
                SizedBox(height: 2.height),
                Text(
                  title,
                  style: const TextStyle(
                    fontSize: 12,
                    color: LiveColors.designStandardG7,
                  ),
                  overflow: TextOverflow.ellipsis,
                ),
              ],
            ),
          ),
        ),
      );

  Widget _buildApplyLinkMicWidget() => GestureDetector(
        onTap: _requestIntraRoomVideoConnection,
        child: ClipRRect(
          borderRadius: BorderRadius.all(Radius.circular(10.radius)),
          child: Container(
            color: LiveColors.designStandardB1,
            width: 200.width,
            height: 52.height,
            child: Center(
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!
                    .common_apply_link_mic,
                style: const TextStyle(
                  fontSize: 16,
                  color: LiveColors.designStandardFlowkitWhite,
                ),
                overflow: TextOverflow.ellipsis,
              ),
            ),
          ),
        ),
      );

  Widget _buildApplyLinkMicTipsWidget() => Center(
        child: Text(
          LiveKitLocalizations.of(Global.appContext())!
              .common_tips_apply_link_mic,
          style: const TextStyle(
            fontSize: 12,
            color: LiveColors.designStandardG4,
          ),
          overflow: TextOverflow.ellipsis,
        ),
      );

  void _showBeautyPanel() {
    popupWidget(const BeautyPanelWidget());
  }
}

extension on _CoGuestVideoSettingsPanelWidgetState {
  void _switchCamera() {
    widget.liveCoreController
        .switchCamera(!widget.liveCoreController.mediaState.isFrontCamera);
  }

  Future<void> _requestIntraRoomVideoConnection() async {
    var result = await widget.liveCoreController.requestIntraRoomConnection(
      widget.liveCoreController.roomState.ownerInfo.userId,
      Constants.defaultRequestTimeout,
      true,
    );
    if (result.code == TUIError.success) {
      makeToast(
        msg: LiveKitLocalizations.of(Global.appContext())!
            .common_toast_apply_link_mic,
      );
    } else {
      makeToast(
          msg: ErrorHandler.convertToErrorMessage(
                  result.code.rawValue, result.message) ??
              '');
    }
    TUILiveKitNavigatorObserver.instance.backToLiveRoomAudiencePage();
  }
}
