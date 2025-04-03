import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/audio_effect/manager/audio_effect_manager.dart';

import '../../../../../common/constants/index.dart';
import '../../../../../common/language/index.dart';
import '../../../../../common/resources/index.dart';
import '../../../../../common/widget/index.dart';
import '../../../../../common/screen/index.dart';

class ReverbWidget extends StatefulWidget {
  final AudioEffectManager manager;

  const ReverbWidget({super.key, required this.manager});

  @override
  State<ReverbWidget> createState() => _ReverbWidgetState();
}

class _ReverbWidgetState extends State<ReverbWidget> {
  late final AudioEffectManager manager;
  List<ReverbItem> mData = [];

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
    _initData();
  }

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      height: context.adapter.getHeight(89),
      child: _initReverbListViewWidget(),
    );
  }

  Widget _initReverbListViewWidget() {
    return ListView.builder(
      shrinkWrap: true,
      physics: const AlwaysScrollableScrollPhysics(),
      scrollDirection: Axis.horizontal,
      itemCount: mData.length,
      itemBuilder: (context, index) {
        return ValueListenableBuilder(
          valueListenable: manager.state.reverbType,
          builder: (context, reverbType, child) {
            return GestureDetector(
              onTap: () {
                _setReverbType(mData[index].type);
              },
              child: Container(
                width: context.adapter.getWidth(56),
                height: context.adapter.getHeight(81),
                margin: EdgeInsets.only(
                    left: context.adapter.getWidth(12),
                    right: context.adapter.getWidth(12)),
                child: Column(
                  children: [
                    Container(
                      width: context.adapter.getWidth(56),
                      padding: EdgeInsets.all(context.adapter.getWidth(13)),
                      decoration: BoxDecoration(
                        color: LiveColors.notStandardBlue30Transparency,
                        border: Border.all(
                            color: reverbType == mData[index].type
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
                    SizedBox(height: context.adapter.getHeight(2)),
                    SizedBox(
                      height: context.adapter.getHeight(18),
                      child: Text(
                        mData[index].title,
                        style: const TextStyle(
                            color: LiveColors.designStandardG6, fontSize: 12),
                      ),
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
        title: LiveKitLocalizations.of(Global.appContext())!.live_reverb_none,
        icon: LiveImages.selectNone,
        type: 0));
    mData.add(ReverbItem(
        title:
            LiveKitLocalizations.of(Global.appContext())!.live_reverb_karaoke,
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

extension on _ReverbWidgetState {
  void _setReverbType(int type) {
    manager.setVoiceReverbType(type);
  }
}
