import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/common/ui_component/audio_effect/service/audio_effect_service.dart';

class ReverbWidget extends BasicWidget {
  const ReverbWidget({super.key, required super.liveController, required this.audioEffectService});

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
          valueListenable: widget.audioEffectService.audioEffectState.reverbType,
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
                      padding: EdgeInsets.all(2),
                      decoration: BoxDecoration(
                        color: LivekitColors.livekitNotStandardBlue30Transparency,
                        border: Border.all(
                            color: widget.audioEffectService.audioEffectState.reverbType.value == mData[index].type
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
    mData.add(ReverbItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_reverb_none,
        icon: LivekitImages.livekitSelectNone,
        type: 0));
    mData.add(ReverbItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_reverb_karaoke,
        icon: LivekitImages.livekitReverbKTV,
        type: 1));
    mData.add(ReverbItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_reverb_metallic_sound,
        icon: LivekitImages.livekitReverbMetallic,
        type: 6));
    mData.add(ReverbItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_reverb_low,
        icon: LivekitImages.livekitReverbLow,
        type: 4));
    mData.add(ReverbItem(
        title: LiveKitLocalizations.of(Global.appContext())!.livekit_reverb_loud_and_loud,
        icon: LivekitImages.livekitReverbLoud,
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
