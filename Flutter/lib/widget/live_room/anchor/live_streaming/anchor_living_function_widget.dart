import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/common/ui_component/music/widget/music_panel_widget.dart';
import 'package:tencent_live_uikit/widget/live_room/anchor/common/settings_panel_widget.dart';
import 'package:tencent_live_uikit/widget/live_room/anchor/live_streaming/link/anchor_link_mic_manage_panel_widget.dart';

class AnchorLivingFunctionWidget extends BasicWidget {
  const AnchorLivingFunctionWidget({super.key, required super.liveController});

  @override
  AnchorLivingFunctionWidgetState getState() {
    return AnchorLivingFunctionWidgetState();
  }
}

class AnchorLivingFunctionWidgetState extends BasicState<AnchorLivingFunctionWidget> {
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
        child: GestureDetector(
            onTap: () {
              _showBarrageSendWidget();
            },
            child: Container(color: Colors.transparent)));
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
            LivekitImages.livekitFunctionLinkDefault,
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
            LivekitImages.livekitFunctionSettings,
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
            LivekitImages.livekitFunctionMusic,
            package: Constants.pluginName,
          ),
        ));
  }
}

extension AnchorLivingFunctionWidgetStateLogicExtension on AnchorLivingFunctionWidgetState {
  _showBarrageSendWidget() {}

  _showLinkPanelWidget() {
    showWidget(AnchorLinkMicManagePanelWidget(liveController: liveController));
  }

  _showMusicPanelWidget() {
    showWidget(MusicPanelWidget(liveController: liveController));
  }

  _showSettingsPanelWidget() {
    showWidget(
      SettingsPanelWidget(liveController: liveController),
      barrierColor: LivekitColors.livekitDesignStandardTransparent,
    );
  }
}
