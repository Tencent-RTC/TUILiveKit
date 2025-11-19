import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/panel/co_guest_video_setting_panel_widget.dart';

class CoGuestTypeSelectPanelWidget extends StatefulWidget {
  final LiveCoreController liveCoreController;
  final int seatIndex;

  const CoGuestTypeSelectPanelWidget({super.key, required this.liveCoreController, this.seatIndex = -1});

  @override
  State<CoGuestTypeSelectPanelWidget> createState() => _CoGuestTypeSelectPanelWidgetState();
}

class _CoGuestTypeSelectPanelWidgetState extends State<CoGuestTypeSelectPanelWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      width: 1.screenWidth,
      height: 234.height,
      decoration: BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(
          topLeft: Radius.circular(20.radius),
          topRight: Radius.circular(20.radius),
        ),
      ),
      child: Column(
        children: [
          _buildTitleWidget(),
          _buildOptionButton(
            icon: LiveImages.linkVideo,
            text: LiveKitLocalizations.of(context)!.common_text_link_mic_video,
            onTap: _connectVideo,
          ),
          _buildOptionButton(
            icon: LiveImages.linkAudio,
            text: LiveKitLocalizations.of(context)!.common_text_link_mic_audio,
            onTap: _connectAudio,
          ),
        ],
      ),
    );
  }

  Widget _buildTitleWidget() {
    double screenWidth = 1.screenWidth;
    return SizedBox(
      height: 89.height,
      width: screenWidth,
      child: Stack(
        children: [
          Positioned(
            top: 20.height,
            child: SizedBox(
              width: screenWidth,
              child: Center(
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!.common_title_link_mic_selector,
                  style: const TextStyle(color: LiveColors.designStandardFlowkitWhite, fontSize: 16),
                ),
              ),
            ),
          ),
          Positioned(
            top: 52.height,
            child: SizedBox(
              width: screenWidth,
              child: Center(
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!.common_text_link_mic_selector,
                  style: const TextStyle(color: LiveColors.notStandardGrey, fontSize: 12),
                ),
              ),
            ),
          ),
          Positioned(
            right: 16.width,
            top: 35.height,
            child: GestureDetector(
              onTap: () {
                _showSettingsPanel();
              },
              child: Container(
                width: 40.width,
                height: 40.height,
                padding: EdgeInsets.all(10.width),
                color: LiveColors.designStandardTransparent,
                child: Image.asset(
                  LiveImages.linkSettings,
                  package: Constants.pluginName,
                ),
              ),
            ),
          )
        ],
      ),
    );
  }

  Widget _buildOptionButton({
    required String icon,
    required String text,
    required VoidCallback onTap,
  }) {
    return GestureDetector(
      onTap: onTap,
      child: Container(
        height: 54.height,
        width: 1.screenWidth,
        color: LiveColors.designStandardTransparent,
        child: Stack(
          children: [
            Container(
              width: 1.width,
              height: 1.height,
              color: LiveColors.notStandardBlue30Transparency,
            ),
            Positioned(
              top: 17.height,
              left: 16.width,
              child: Image.asset(
                icon,
                package: Constants.pluginName,
                width: 20.width,
                height: 20.height,
              ),
            ),
            Positioned(
              top: 17.height,
              left: 49.width,
              child: Text(
                text,
                style: const TextStyle(
                  color: LiveColors.designStandardFlowkitWhite,
                  fontSize: 16,
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }

  void _showSettingsPanel() {
    Navigator.of(Global.appContext()).pop();
    popupWidget(CoGuestVideoSettingsPanelWidget(
      liveCoreController: widget.liveCoreController,
      seatIndex: widget.seatIndex,
    ));
  }
}

extension on _CoGuestTypeSelectPanelWidgetState {
  void _connectVideo() => _requestConnection(isVideo: true);

  void _connectAudio() => _requestConnection(isVideo: false);

  Future<void> _requestConnection({required bool isVideo}) async {
    var result = await widget.liveCoreController.requestIntraRoomConnection(
      userId: widget.liveCoreController.roomState.ownerInfo.userId,
      seatIndex: widget.seatIndex,
      timeout: Constants.defaultRequestTimeout,
      openCamera: isVideo,
    );
    if (result.code == TUIError.success) {
      makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.common_toast_apply_link_mic);
    } else {
      makeToast(msg: ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
    }
    Navigator.of(Global.appContext()).pop();
  }
}
