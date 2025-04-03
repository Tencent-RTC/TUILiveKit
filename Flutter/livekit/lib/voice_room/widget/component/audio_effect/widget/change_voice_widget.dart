import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/audio_effect/manager/audio_effect_manager.dart';

import '../../../../../common/constants/index.dart';
import '../../../../../common/language/index.dart';
import '../../../../../common/resources/index.dart';
import '../../../../../common/widget/index.dart';
import '../../../../../common/screen//index.dart';

class ChangeVoiceWidget extends StatefulWidget {
  final AudioEffectManager manager;

  const ChangeVoiceWidget({super.key, required this.manager});

  @override
  State<ChangeVoiceWidget> createState() => _ChangeVoiceWidgetState();
}

class _ChangeVoiceWidgetState extends State<ChangeVoiceWidget> {
  late final AudioEffectManager manager;
  List<ChangeVoiceItem> mData = [];

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
      child: _initChangeVoiceListViewWidget(),
    );
  }

  Widget _initChangeVoiceListViewWidget() {
    return ListView.builder(
      shrinkWrap: true,
      physics: const AlwaysScrollableScrollPhysics(),
      scrollDirection: Axis.horizontal,
      itemCount: mData.length,
      itemBuilder: (context, index) {
        return ValueListenableBuilder(
          valueListenable: manager.state.changerType,
          builder: (context, changerType, child) {
            return GestureDetector(
              onTap: () {
                _changeVoice(mData[index].type);
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
                      height: context.adapter.getWidth(56),
                      padding: EdgeInsets.all(context.adapter.getWidth(13)),
                      decoration: BoxDecoration(
                        color: LiveColors.notStandardBlue30Transparency,
                        border: Border.all(
                            color: changerType == mData[index].type
                                ? LiveColors.designStandardB1
                                : LiveColors.notStandardBlue30Transparency,
                            width: context.adapter.getWidth(2)),
                        borderRadius:
                            BorderRadius.circular(context.adapter.getWidth(10)),
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
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .live_change_voice_none,
        icon: LiveImages.selectNone,
        type: 0));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .live_change_voice_child,
        icon: LiveImages.changeVoiceChild,
        type: 1));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .live_change_voice_girl,
        icon: LiveImages.changeVoiceGirl,
        type: 2));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .live_change_voice_uncle,
        icon: LiveImages.changeVoiceUncle,
        type: 3));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .live_change_voice_ethereal,
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

extension on _ChangeVoiceWidgetState {
  void _changeVoice(int type) {
    manager.setVoiceChangerType(type);
  }
}
