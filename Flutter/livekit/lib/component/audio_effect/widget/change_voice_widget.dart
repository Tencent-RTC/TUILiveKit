import 'package:flutter/material.dart';

import '../../../../../common/constants/index.dart';
import '../../../../../common/language/index.dart';
import '../../../../../common/resources/index.dart';
import '../../../../../common/widget/index.dart';
import '../../../../../common/screen//index.dart';
import '../manager/audio_effect_manager.dart';

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
      height: 89.height,
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
                width: 56.width,
                height: 81.height,
                margin: EdgeInsets.only(left: 12.width, right: 12.width),
                child: Column(
                  children: [
                    Container(
                      width: 56.radius,
                      height: 56.radius,
                      padding: EdgeInsets.all(13.width),
                      decoration: BoxDecoration(
                        color: LiveColors.notStandardBlue30Transparency,
                        border: Border.all(
                            color: changerType == mData[index].type
                                ? LiveColors.designStandardB1
                                : LiveColors.notStandardBlue30Transparency,
                            width: 2.width),
                        borderRadius: BorderRadius.circular(10.radius),
                      ),
                      child: Image.asset(
                        mData[index].icon,
                        package: Constants.pluginName,
                      ),
                    ),
                    SizedBox(height: 2.height),
                    SizedBox(
                      height: 18.height,
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
            .common_change_voice_none,
        icon: LiveImages.selectNone,
        type: 0));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .common_change_voice_child,
        icon: LiveImages.changeVoiceChild,
        type: 1));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .common_change_voice_girl,
        icon: LiveImages.changeVoiceGirl,
        type: 2));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .common_change_voice_uncle,
        icon: LiveImages.changeVoiceUncle,
        type: 3));
    mData.add(ChangeVoiceItem(
        title: LiveKitLocalizations.of(Global.appContext())!
            .common_change_voice_ethereal,
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
