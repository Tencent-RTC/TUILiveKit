import 'package:flutter/material.dart';

import '../../../common/index.dart';
import '../service/audio_effect_service.dart';

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
                        color: LiveColors.notStandardBlue30Transparency,
                        border: Border.all(
                            color: widget.audioEffectService.audioEffectState.changerType.value == mData[index].type
                                ? LiveColors.designStandardB1
                                : LiveColors.notStandardBlue30Transparency,
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
                      style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 12),
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
        title: LiveKitLocalizations.of(Global.appContext())!.live_change_voice_none,
        icon: LiveImages.selectNone,
        type: 0));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!.live_change_voice_child,
        icon: LiveImages.changeVoiceChild,
        type: 1));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!.live_change_voice_girl,
        icon: LiveImages.changeVoiceGirl,
        type: 2));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!.live_change_voice_uncle,
        icon: LiveImages.changeVoiceUncle,
        type: 3));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!.live_change_voice_ethereal,
        icon: LiveImages.changeVoiceEthereal,
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
