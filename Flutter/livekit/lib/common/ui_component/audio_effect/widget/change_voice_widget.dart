import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/common/ui_component/audio_effect/service/audio_effect_service.dart';

class ChangeVoiceWidget extends BasicWidget {
  const ChangeVoiceWidget({super.key, required super.liveController, required this.audioEffectService});

  final AudioEffectService audioEffectService;

  @override
  ChangeVoiceWidgetState getState() {
    return ChangeVoiceWidgetState();
  }
}

class ChangeVoiceWidgetState extends BasicState<ChangeVoiceWidget> {
  List<ChangeVoiceItem> mData = [];

  @override
  void initState() {
    super.initState();
    _initData();
  }

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      height: 80,
      child: _initChangeVoiceListViewWidget(),
    );
  }

  _initChangeVoiceListViewWidget() {
    return ListView.builder(
      shrinkWrap: true,
      physics: const AlwaysScrollableScrollPhysics(),
      scrollDirection: Axis.horizontal,
      itemCount: mData.length,
      itemBuilder: (context, index) {
        return ValueListenableBuilder(
          valueListenable: widget.audioEffectService.audioEffectState.changerType,
          builder: (BuildContext context, int value, Widget? child) {
            return GestureDetector(
              onTap: () {
                _changeVoice(mData[index].type);
              },
              child: Container(
                width: 56,
                height: 79,
                margin: const EdgeInsets.only(left: 15, right: 10),
                child: Column(
                  children: [
                    Container(
                      width: 56,
                      height: 56,
                      padding: const EdgeInsets.all(2),
                      decoration: BoxDecoration(
                        color: LivekitColors.livekitNotStandardBlue30Transparency,
                        border: Border.all(
                            color: widget.audioEffectService.audioEffectState.changerType.value == mData[index].type
                                ? LivekitColors.livekitDesignStandardB1
                                : LivekitColors.livekitNotStandardBlue30Transparency,
                            width: 2),
                        borderRadius: BorderRadius.circular(10),
                      ),
                      child: Image.asset(
                        mData[index].icon,
                        package: Constants.pluginName,
                      ),
                    ),
                    const SizedBox(
                      height: 2,
                    ),
                    Text(
                      mData[index].title,
                      style: const TextStyle(color: LivekitColors.livekitDesignStandardG6, fontSize: 12),
                    ),
                  ],
                ),
              ),
            );
          },
        );
      },
    );
  }

  void _initData() {
    mData.clear();
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_change_voice_none,
        icon: LivekitImages.livekitSelectNone,
        type: 0));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_change_voice_child,
        icon: LivekitImages.livekitChangeVoiceChild,
        type: 1));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_change_voice_girl,
        icon: LivekitImages.livekitChangeVoiceGirl,
        type: 2));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_change_voice_uncle,
        icon: LivekitImages.livekitChangeVoiceUncle,
        type: 3));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_change_voice_ethereal,
        icon: LivekitImages.livekitChangeVoiceEthereal,
        type: 11));
  }
}

class ChangeVoiceItem {
  String title;
  String icon;
  int type;

  ChangeVoiceItem({
    required this.title,
    required this.icon,
    required this.type,
  });
}

extension ChangeVoiceWidgetStateLogicExtension on ChangeVoiceWidgetState {
  _changeVoice(int type) {
    widget.audioEffectService.setVoiceChangerType(type);
  }
}
