import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:flutter/material.dart';

import '../../../../../common/index.dart';
import '../common/settings_panel_widget.dart';
import 'link/anchor_link_mic_manage_panel_widget.dart';

class AnchorLivingFunctionWidget extends BasicWidget {
  const AnchorLivingFunctionWidget({super.key, required super.liveController});

  @override
  AnchorLivingFunctionWidgetState getState() {
    return AnchorLivingFunctionWidgetState();
  }
}

class AnchorLivingFunctionWidgetState extends BasicState<AnchorLivingFunctionWidget> {
  late BarrageSendController _barrageSendController;

  @override
  Widget build(BuildContext context) {
    return Stack(children: [
      _initBarrageSendWidget(),
      _initLinkWidget(),
      _initSettingsWidget(),
      _initMusicWidget(),
    ]);
  }

  _initBarrageSendWidget() {
    return Positioned(
        left: 15,
        top: 0,
        width: 130,
        height: 36,
        child: ValueListenableBuilder(
          valueListenable: liveController.getRoomSate().enterRoomSuccess,
          builder: (BuildContext context, bool value, Widget? child) {
            if (liveController.getRoomSate().enterRoomSuccess.value) {
              _barrageSendController = BarrageSendController(
                  roomId: liveController.getRoomSate().roomId,
                  ownerId: liveController.getRoomSate().ownerInfo.userId,
                  selfUserId: liveController.getUserState().selfInfo.userId,
                  selfName: liveController.getUserState().selfInfo.name.value);
              return BarrageSendWidget(controller: _barrageSendController);
            } else {
              return Container();
            }
          },
        ));
  }

  _initLinkWidget() {
    return Positioned(
        right: 100,
        top: 2,
        width: 32,
        height: 32,
        child: GestureDetector(
          onTap: () {
            _showLinkPanelWidget();
          },
          child: Image.asset(
            LiveImages.functionLinkDefault,
            package: Constants.pluginName,
          ),
        ));
  }

  _initSettingsWidget() {
    return Positioned(
        right: 60,
        top: 2,
        width: 32,
        height: 32,
        child: GestureDetector(
          onTap: () {
            _showSettingsPanelWidget();
          },
          child: Image.asset(
            LiveImages.functionSettings,
            package: Constants.pluginName,
          ),
        ));
  }

  _initMusicWidget() {
    return Positioned(
        right: 20,
        top: 2,
        width: 32,
        height: 32,
        child: GestureDetector(
          onTap: () {
            _showMusicPanelWidget();
          },
          child: Image.asset(
            LiveImages.functionMusic,
            package: Constants.pluginName,
          ),
        ));
  }
}

extension AnchorLivingFunctionWidgetStateLogicExtension on AnchorLivingFunctionWidgetState {
  _showLinkPanelWidget() {
    showWidget(AnchorLinkMicManagePanelWidget(liveController: liveController));
  }

  _showMusicPanelWidget() {
    showWidget(MusicPanelWidget(liveController: liveController));
  }

  _showSettingsPanelWidget() {
    showWidget(
      SettingsPanelWidget(liveController: liveController),
      barrierColor: LiveColors.designStandardTransparent,
    );
  }
}
