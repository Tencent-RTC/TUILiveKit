import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/widget/live_room/audience/component/live_streaming/link/video_link_settings_panel_widget.dart';

class SelectLinkMicTypePanelWidget extends BasicWidget {
  const SelectLinkMicTypePanelWidget({super.key, required super.liveController});

  @override
  SelectLinkMicTypePanelWidgetState getState() {
    return SelectLinkMicTypePanelWidgetState();
  }
}

class SelectLinkMicTypePanelWidgetState extends BasicState<SelectLinkMicTypePanelWidget> {
  static const int seatIndex = -1;

  @override
  Widget build(BuildContext context) {
    return Container(
      width: screenWidth,
      height: 375,
      decoration: const BoxDecoration(
        color: LivekitColors.livekitDesignStandardG2,
        borderRadius: BorderRadius.only(topLeft: Radius.circular(20), topRight: Radius.circular(20)),
      ),
      child: Column(children: [
        _initLinkSettingsTitleWidget(),
        _initLinkVideoWidget(),
        _initLinkAudioWidget(),
      ]),
    );
  }

  Widget _initLinkSettingsTitleWidget() {
    return SizedBox(
      height: 89,
      width: screenWidth,
      child: Stack(
        children: [
          Positioned(
            top: 20,
            child: SizedBox(
              width: screenWidth,
              child: Center(
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!.livekit_title_link_mic_selector,
                  style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 16),
                ),
              ),
            ),
          ),
          Positioned(
            top: 52,
            child: SizedBox(
              width: screenWidth,
              child: Center(
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!.livekit_text_link_mic_selector,
                  style: const TextStyle(color: LivekitColors.livekitNotStandardGrey, fontSize: 12),
                ),
              ),
            ),
          ),
          Positioned(
            right: 16,
            top: 35,
            child: GestureDetector(
              onTap: () {
                _showSettingsPanelWidget();
              },
              child: Container(
                width: 40,
                height: 40,
                padding: const EdgeInsets.all(10),
                color: LivekitColors.livekitDesignStandardTransparent,
                child: Image.asset(
                  LivekitImages.livekitLinkSettings,
                  package: Constants.pluginName,
                ),
              ),
            ),
          )
        ],
      ),
    );
  }

  Widget _initLinkVideoWidget() {
    return GestureDetector(
      onTap: () {
        _linkVideo();
      },
      child: Container(
        height: 54,
        width: screenWidth,
        color: LivekitColors.livekitDesignStandardTransparent,
        child: Stack(
          children: [
            Container(
              width: screenWidth,
              height: 1,
              color: LivekitColors.livekitNotStandardBlue30Transparency,
            ),
            Positioned(
              top: 17,
              left: 16,
              child: SizedBox(
                width: 20,
                height: 20,
                child: Image.asset(
                  LivekitImages.livekitLinkVideo,
                  package: Constants.pluginName,
                ),
              ),
            ),
            Positioned(
              top: 17,
              left: 49,
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!.livekit_text_link_mic_video,
                style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 16),
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _initLinkAudioWidget() {
    return GestureDetector(
      onTap: () {
        _linkAudio();
      },
      child: Container(
        height: 54,
        width: screenWidth,
        color: LivekitColors.livekitDesignStandardTransparent,
        child: Stack(
          children: [
            Container(
              width: screenWidth,
              height: 1,
              color: LivekitColors.livekitNotStandardBlue30Transparency,
            ),
            Positioned(
              top: 17,
              left: 16,
              child: SizedBox(
                width: 20,
                height: 20,
                child: Image.asset(
                  LivekitImages.livekitLinkAudio,
                  package: Constants.pluginName,
                ),
              ),
            ),
            Positioned(
              top: 17,
              left: 49,
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!.livekit_text_link_mic_audio,
                style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 16),
              ),
            ),
          ],
        ),
      ),
    );
  }
}

extension SelectLinkMicTypePanelWidgetStateLogicExtension on SelectLinkMicTypePanelWidgetState {
  void _showSettingsPanelWidget() {
    showWidget(VideoLinkSettingsPanelWidget(liveController: liveController));
  }

  void _linkVideo() {
    liveController.viewController.enableAutoOpenCameraOnSeated(true);
    makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.livekit_toast_apply_link_mic);
    liveController.seatController.takeSeat(SelectLinkMicTypePanelWidgetState.seatIndex);
    Navigator.of(context).pop();
  }

  void _linkAudio() {
    liveController.viewController.enableAutoOpenCameraOnSeated(false);
    makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.livekit_toast_apply_link_mic);
    liveController.seatController.takeSeat(SelectLinkMicTypePanelWidgetState.seatIndex);
    Navigator.of(context).pop();
  }
}
