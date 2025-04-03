import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../../../../../../../common/index.dart';
import '../../../../../../../live_navigator_observer.dart';
import 'select_link_mic_type_panel_widget.dart';

class VideoLinkSettingsPanelWidget extends BasicWidget {
  const VideoLinkSettingsPanelWidget({super.key, required super.liveController});

  @override
  VideoLinkSettingsPanelWidgetState getState() {
    return VideoLinkSettingsPanelWidgetState();
  }
}

class VideoLinkSettingsPanelWidgetState extends BasicState<VideoLinkSettingsPanelWidget> {
  late final ValueNotifier<int> selectIndex = ValueNotifier(0);
  late final ValueNotifier<int> sliderValue = ValueNotifier(0);
  late final List<BeautyItem> list;

  @override
  void dispose() {
    liveController.mediaController.closeLocalCamera();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      width: screenWidth,
      height: !liveController.isOwner() ? 700 : 350,
      decoration: const BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(topLeft: Radius.circular(20), topRight: Radius.circular(20)),
      ),
      child: Column(children: [
        _initTitleWidget(),
        32.verticalSpace,
        _initVideoWidget(),
        24.verticalSpace,
        _initFunctionWidget(),
        110.verticalSpace,
        _initApplyLinkMicWidget(),
        20.verticalSpace,
        _initApplyLinkMicTipsWidget(),
      ]),
    );
  }

  _initTitleWidget() {
    return SizedBox(
      height: 44,
      width: screenWidth,
      child: Stack(
        children: [
          Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!.live_title_link_video_settings,
              style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  _initVideoWidget() {
    return ClipRRect(
      borderRadius: const BorderRadius.all(Radius.circular(16)),
      child: SizedBox(
          width: 328,
          height: 328,
          child: VideoView(
            onViewCreated: (id) {
              liveController.mediaController.setLocalVideoView(id);
              liveController.mediaController.openLocalCamera();
            },
          )),
    );
  }

  _initFunctionWidget() {
    return Row(
      mainAxisAlignment: MainAxisAlignment.center,
      children: [
        _initBeautyWidget(),
        12.horizontalSpace,
        _initFlipWidget(),
        12.horizontalSpace,
        _initMirrorWidget(),
      ],
    );
  }

  Widget _initBeautyWidget() {
    return GestureDetector(
      onTap: () {
        _showBeautyPanel();
      },
      child: _buttonContainerWidget(LiveImages.videoSettingsBeauty,
          LiveKitLocalizations.of(Global.appContext())!.live_video_settings_item_beauty),
    );
  }

  Widget _initFlipWidget() {
    return GestureDetector(
      onTap: () {
        _flipCamera();
      },
      child: _buttonContainerWidget(LiveImages.videoSettingsFlip,
          LiveKitLocalizations.of(Global.appContext())!.live_video_settings_item_flip),
    );
  }

  Widget _initMirrorWidget() {
    return GestureDetector(
      onTap: () {
        _setVideoMirror();
      },
      child: _buttonContainerWidget(LiveImages.videoSettingsMirror,
          LiveKitLocalizations.of(Global.appContext())!.live_video_settings_item_mirror),
    );
  }

  Widget _initApplyLinkMicWidget() {
    return GestureDetector(
      onTap: () {
        _linkVideo();
      },
      child: ClipRRect(
        borderRadius: const BorderRadius.all(Radius.circular(10)),
        child: Container(
          color: LiveColors.designStandardB1,
          width: 200,
          height: 52,
          child: Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!.live_btn_apply_link_mic,
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
  }

  Widget _initApplyLinkMicTipsWidget() {
    return Center(
      child: Text(
        LiveKitLocalizations.of(Global.appContext())!.live_tips_apply_link_mic,
        style: const TextStyle(
          fontSize: 12,
          color: LiveColors.designStandardG4,
        ),
        overflow: TextOverflow.ellipsis,
      ),
    );
  }
}

extension VideoLinkSettingsPanelWidgetLogicExtension on VideoLinkSettingsPanelWidgetState {
  void _showBeautyPanel() {
    showWidget(BeautyPanelWidget(liveController: liveController),
        barrierColor: LiveColors.designStandardTransparent);
  }

  void _flipCamera() {
    liveController.mediaController.switchCamera();
  }

  void _setVideoMirror() {
    liveController.mediaController.setCameraMirror();
  }

  void _linkVideo() {
    liveController.viewController.enableAutoOpenCameraOnSeated(true);
    makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.live_toast_apply_link_mic);
    liveController.seatController.takeSeat(SelectLinkMicTypePanelWidgetState.seatIndex);
    TUILiveKitNavigatorObserver.instance.backToLiveRoomAudiencePage();
  }

  Widget _buttonContainerWidget(String image, String title) {
    return ClipRRect(
      borderRadius: const BorderRadius.all(Radius.circular(10)),
      child: Container(
        color: LiveColors.designStandardG3,
        width: 56,
        height: 56,
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            SizedBox(
              width: 24,
              height: 24,
              child: Image.asset(
                image,
                package: Constants.pluginName,
              ),
            ),
            2.verticalSpace,
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
    );
  }
}
