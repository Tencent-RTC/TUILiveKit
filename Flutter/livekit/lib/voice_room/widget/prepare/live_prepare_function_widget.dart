import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/audio_effect/audio_effect_panel_widget.dart';

import '../../../common/constants/index.dart';
import '../../../common/language/index.dart';
import '../../../common/resources/index.dart';
import '../../../common/widget/index.dart';
import '../../../common/screen/index.dart';

class LivePrepareFunctionWidget extends StatefulWidget {
  final VoiceRoomManager manager;

  const LivePrepareFunctionWidget({super.key, required this.manager});

  @override
  State<LivePrepareFunctionWidget> createState() =>
      _LivePrepareFunctionWidgetState();
}

class _LivePrepareFunctionWidgetState extends State<LivePrepareFunctionWidget> {
  late final VoiceRoomManager manager;

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
  }

  @override
  Widget build(BuildContext context) {
    return Row(mainAxisAlignment: MainAxisAlignment.spaceEvenly, children: [
      _initBackgroundSelectorWidget(),
      _initAudioEffectWidget(),
      _initSettingsWidget(),
    ]);
  }

  Widget _initBackgroundSelectorWidget() {
    return GestureDetector(
      onTap: () {
        _showBackgroundSelectorPanel();
      },
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: context.adapter.getWidth(36),
            height: context.adapter.getWidth(36),
            child: Image.asset(
              LiveImages.voiceBackground,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!
                .live_settings_bg_image,
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

  Widget _initAudioEffectWidget() {
    return GestureDetector(
      onTap: () {
        _showAudioEffectPanel();
      },
      child: Column(
        mainAxisSize: MainAxisSize.min,
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: context.adapter.getWidth(36),
            height: context.adapter.getWidth(36),
            child: Image.asset(
              LiveImages.prepareAudio,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_audio_effect,
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

  Widget _initSettingsWidget() {
    return GestureDetector(
      onTap: () {
        _showSettingsPanel();
      },
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: context.adapter.getWidth(36),
            height: context.adapter.getWidth(36),
            child: Image.asset(
              LiveImages.prepareSetting,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_settings,
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

extension on _LivePrepareFunctionWidgetState {
  void _showBackgroundSelectorPanel() {
    popupWidget(LiveBackgroundSelectPanelWidget(
        manager: manager,
        backgroundUrls: Constants.backgroundUrlList,
        initialBackgroundUrl: manager.roomState.backgroundUrl.value));
  }

  void _showAudioEffectPanel() {
    popupWidget(AudioEffectPanelWidget(roomId: manager.roomState.roomId));
  }

  void _showSettingsPanel() {
    popupWidget(SeatModeSettingPanelWidget(manager: manager));
  }
}
