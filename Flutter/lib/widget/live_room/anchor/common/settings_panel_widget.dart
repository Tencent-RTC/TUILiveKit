import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

import 'video_params_panel_widget.dart';

class SettingsPanelWidget extends BasicWidget {
  const SettingsPanelWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return SettingsPanelWidgetState();
  }
}

class SettingsPanelWidgetState extends BasicState<SettingsPanelWidget> {
  late final List<SettingsItem> list;

  @override
  void initState() {
    super.initState();
    _initData();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      width: screenWidth,
      height: 350,
      decoration: const BoxDecoration(
        color: LivekitColors.livekitDesignStandardG2,
        borderRadius: BorderRadius.only(topLeft: Radius.circular(20), topRight: Radius.circular(20)),
      ),
      child: Column(children: [_initTitleWidget(), 24.verticalSpace, _initSettingsListWidget()]),
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
              LiveKitLocalizations.of(Global.appContext())!.livekit_settings,
              style: const TextStyle(color: LivekitColors.livekitDesignStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  _initSettingsListWidget() {
    return SizedBox(
      width: screenWidth,
      height: 79,
      child: Center(
        child: ListView.builder(
          shrinkWrap: true,
          physics: const AlwaysScrollableScrollPhysics(),
          scrollDirection: Axis.horizontal,
          itemCount: list.length,
          itemBuilder: (context, index) {
            return GestureDetector(
              onTap: () => _onTapIndex(index),
              child: Container(
                width: 56,
                height: 79,
                margin: const EdgeInsets.symmetric(horizontal: 6),
                child: Column(
                  children: [
                    Container(
                      width: 56,
                      height: 56,
                      padding: const EdgeInsets.all(2),
                      decoration: BoxDecoration(
                        color: LivekitColors.livekitNotStandardBlue30Transparency,
                        border: Border.all(color: LivekitColors.livekitNotStandardBlue30Transparency, width: 2),
                        borderRadius: BorderRadius.circular(10),
                      ),
                      child: Center(
                        child: SizedBox(
                          width: 28,
                          height: 28,
                          child: Image.asset(
                            list[index].icon,
                            package: Constants.pluginName,
                          ),
                        ),
                      ),
                    ),
                    2.verticalSpace,
                    Text(
                      list[index].title,
                      style: const TextStyle(color: LivekitColors.livekitDesignStandardG6, fontSize: 12),
                    ),
                  ],
                ),
              ),
            );
          },
        ),
      ),
    );
  }
}

extension SettingsPanelWidgetStateLogicExtension on SettingsPanelWidgetState {
  void _onTapIndex(int index) {
    final item = list[index];
    switch (item.type) {
      case SettingsItemType.beauty:
        showWidget(
          BeautyPanelWidget(liveController: liveController),
          barrierColor: LivekitColors.livekitDesignStandardTransparent,
        );
        break;
      case SettingsItemType.audioEffect:
        showWidget(AudioEffectPanelWidget(liveController: liveController));
        break;
      case SettingsItemType.flip:
        liveController.mediaController.switchCamera();
        break;
      case SettingsItemType.mirror:
        liveController.mediaController.setCameraMirror();
        break;
      case SettingsItemType.videoParams:
        showWidget(
          VideoParamsPanelWidget(liveController: liveController),
          barrierColor: LivekitColors.livekitDesignStandardTransparent,
        );
        break;
      default:
        break;
    }
  }

  void _initData() {
    list = [
      SettingsItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_function_item_beauty,
          icon: LivekitImages.livekitSettingsItemBeauty,
          type: SettingsItemType.beauty),
      SettingsItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_audio_effect,
          icon: LivekitImages.livekitSettingsItemMusic,
          type: SettingsItemType.audioEffect),
      SettingsItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_function_item_flip,
          icon: LivekitImages.livekitSettingsItemFlip,
          type: SettingsItemType.flip),
      SettingsItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_function_item_mirror,
          icon: LivekitImages.livekitSettingsItemMirror,
          type: SettingsItemType.mirror),
      SettingsItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_video_params,
          icon: LivekitImages.livekitSettingsItemVideoParams,
          type: SettingsItemType.videoParams),
    ];
  }
}

enum SettingsItemType { beauty, audioEffect, flip, mirror, videoParams }

class SettingsItem {
  String title;
  String icon;
  SettingsItemType type;

  SettingsItem({
    required this.title,
    required this.icon,
    required this.type,
  });
}
