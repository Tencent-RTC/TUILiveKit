import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/widget/index.dart';

import '../../../common/constants/constants.dart';
import '../../../common/language/index.dart';
import '../../../common/resources/index.dart';
import '../../../common/screen/index.dart';
import '../../../component/audio_effect/index.dart';
import '../../manager/index.dart';
import 'index.dart';

class SettingsPanelWidget extends StatefulWidget {
  final VoiceRoomManager manager;

  const SettingsPanelWidget({super.key, required this.manager});

  @override
  State<SettingsPanelWidget> createState() => _SettingsPanelWidgetState();
}

class _SettingsPanelWidgetState extends State<SettingsPanelWidget> {
  late final VoiceRoomManager manager;
  late final List<_SettingsItem> list;
  late double _screenWidth;

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
    _initSettingItems();
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.sizeOf(context).width;
    return Container(
      width: _screenWidth,
      height: 350.height,
      decoration: BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(
            topLeft: Radius.circular(20.width),
            topRight: Radius.circular(20.width)),
      ),
      child: Column(children: [
        SizedBox(height: 20.height),
        _initTitleWidget(),
        SizedBox(height: 32.height),
        _initSettingsListWidget()
      ]),
    );
  }

  Widget _initTitleWidget() {
    return Container(
      alignment: Alignment.center,
      height: 28.height,
      width: _screenWidth,
      child: Text(
        LiveKitLocalizations.of(Global.appContext())!.common_settings,
        style:
            const TextStyle(color: LiveColors.designStandardG7, fontSize: 16),
      ),
    );
  }

  Widget _initSettingsListWidget() {
    return SizedBox(
      width: _screenWidth,
      height: 92.height,
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
                width: 56.width,
                height: 79.height,
                margin: EdgeInsets.symmetric(horizontal: 22.width),
                child: Column(
                  children: [
                    Container(
                      width: 56.radius,
                      height: 56.radius,
                      padding: EdgeInsets.all(2.radius),
                      decoration: BoxDecoration(
                        color: LiveColors.notStandardBlue30Transparency,
                        border: Border.all(
                            color: LiveColors.notStandardBlue30Transparency,
                            width: 2.width),
                        borderRadius: BorderRadius.circular(10.radius),
                      ),
                      child: Center(
                        child: SizedBox(
                          width: 30.radius,
                          height: 30.radius,
                          child: Image.asset(
                            list[index].icon,
                            package: Constants.pluginName,
                          ),
                        ),
                      ),
                    ),
                    SizedBox(height: 6.height),
                    Text(
                      list[index].title,
                      style: const TextStyle(
                          color: LiveColors.designStandardG6, fontSize: 12),
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

extension on _SettingsPanelWidgetState {
  void _onTapIndex(int index) {
    final item = list[index];
    switch (item.type) {
      case _SettingsItemType.background:
        popupWidget(LiveBackgroundSelectPanelWidget(
            manager: manager,
            backgroundUrls: Constants.backgroundUrlList,
            initialBackgroundUrl: manager.roomState.backgroundUrl.value));
        break;
      case _SettingsItemType.audioEffect:
        popupWidget(AudioEffectPanelWidget(roomId: manager.roomState.roomId));
        break;
      default:
        break;
    }
  }

  void _initSettingItems() {
    list = [
      _SettingsItem(
          title: LiveKitLocalizations.of(Global.appContext())!
              .common_settings_bg_image,
          icon: LiveImages.settingBackground,
          type: _SettingsItemType.background),
      _SettingsItem(
          title:
              LiveKitLocalizations.of(Global.appContext())!.common_audio_effect,
          icon: LiveImages.settingsItemMusic,
          type: _SettingsItemType.audioEffect)
    ];
  }
}

enum _SettingsItemType { background, audioEffect }

class _SettingsItem {
  String title;
  String icon;
  _SettingsItemType type;

  _SettingsItem({
    required this.title,
    required this.icon,
    required this.type,
  });
}
