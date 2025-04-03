import 'package:flutter/material.dart';

import '../../../common/index.dart';
import '../service/audio_effect_service.dart';

class ReverbWidget extends BasicWidget {
  const ReverbWidget(
      {super.key,
      required super.liveController,
      required this.audioEffectService});

  final AudioEffectService audioEffectService;

  @override
  ReverbWidgetState getState() {
    return ReverbWidgetState();
  }
}

class ReverbWidgetState extends BasicState<ReverbWidget> {
  List<ReverbItem> mData = [];

  @override
  void initState() {
    super.initState();
    _initData();
  }

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      height: 80,
      child: _initReverbListViewWidget(),
    );
  }

  _initReverbListViewWidget() {
    return ListView.builder(
      shrinkWrap: true,
      physics: const AlwaysScrollableScrollPhysics(),
      scrollDirection: Axis.horizontal,
      itemCount: mData.length,
      itemBuilder: (context, index) {
        return ValueListenableBuilder(
          valueListenable:
              widget.audioEffectService.audioEffectState.reverbType,
          builder: (BuildContext context, int value, Widget? child) {
            return GestureDetector(
              onTap: () {
                _setReverbType(mData[index].type);
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
                        color:
                            LiveColors.notStandardBlue30Transparency,
                        border: Border.all(
                            color: widget.audioEffectService.audioEffectState
                                        .reverbType.value ==
                                    mData[index].type
                                ? LiveColors.designStandardB1
                                : LiveColors
                                    .notStandardBlue30Transparency,
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
                      style: const TextStyle(
                          color: LiveColors.designStandardG6,
                          fontSize: 12),
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
    mData.add(ReverbItem(
        title:
            LiveKitLocalizations.of(Global.appContext())!.live_reverb_none,
        icon: LiveImages.selectNone,
        type: 0));
    mData.add(ReverbItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .live_reverb_karaoke,
        icon: LiveImages.reverbKTV,
        type: 1));
    mData.add(ReverbItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .live_reverb_metallic_sound,
        icon: LiveImages.reverbMetallic,
        type: 6));
    mData.add(ReverbItem(
        title: LiveKitLocalizations.of(Global.appContext())!.live_reverb_low,
        icon: LiveImages.reverbLow,
        type: 4));
    mData.add(ReverbItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .live_reverb_loud_and_loud,
        icon: LiveImages.reverbLoud,
        type: 5));
  }
}

class ReverbItem {
  String title;
  String icon;
  int type;

  ReverbItem({
    required this.title,
    required this.icon,
    required this.type,
  });
}

extension ReverbWidgetStateLogicExtension on ReverbWidgetState {
  _setReverbType(int type) {
    widget.audioEffectService.setVoiceReverbType(type);
  }
}
