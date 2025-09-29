import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

import 'manager/beauty_manager.dart';
import 'state/beautyStateFactory.dart';
import 'state/beauty_state.dart';

class BeautyPanelWidget extends StatefulWidget {
  const BeautyPanelWidget({super.key});

  @override
  State<BeautyPanelWidget> createState() => _BeautyPanelWidgetState();
}

class _BeautyPanelWidgetState extends State<BeautyPanelWidget> {
  late final BeautyManager manager;
  late final ValueNotifier<int> selectIndex = ValueNotifier(0);
  late final ValueNotifier<int> sliderValue = ValueNotifier(0);
  late final List<BeautyItem> list;

  @override
  void initState() {
    super.initState();
    _initData();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      width: 1.screenWidth,
      height: 310.height,
      decoration: BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(
            topLeft: Radius.circular(20.radius),
            topRight: Radius.circular(20.radius)),
      ),
      child: Column(children: [
        SizedBox(height: 20.height),
        _initTitleWidget(),
        SizedBox(height: 32.height),
        _initSliderWidget(),
        SizedBox(height: 16.height),
        _initBeautyListWidget()
      ]),
    );
  }

  Widget _initTitleWidget() {
    return SizedBox(
      height: 24.height,
      width: 1.screenWidth,
      child: Stack(
        children: [
          Positioned(
            left: 24.width,
            child: GestureDetector(
              onTap: () {
                Navigator.pop(context);
              },
              child: SizedBox(
                width: 24.height,
                height: 24.height,
                child: Image.asset(
                  LiveImages.returnArrow,
                  package: Constants.pluginName,
                ),
              ),
            ),
          ),
          Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!
                  .common_beauty_panel_title,
              style: const TextStyle(
                  color: LiveColors.designStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  Widget _initSliderWidget() {
    return ValueListenableBuilder(
      valueListenable: selectIndex,
      builder: (BuildContext context, int value, Widget? child) {
        if (selectIndex.value == 0) {
          return SizedBox(height: 22.height);
        } else {
          return ValueListenableBuilder(
            valueListenable: sliderValue,
            builder: (context, sliderValue, _) {
              return SizedBox(
                height: 22.height,
                child: Row(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    Text(
                      _getLeftTitle(),
                      style: const TextStyle(
                          color: LiveColors.designStandardG6, fontSize: 12),
                    ),
                    SizedBox(width: 9.width),
                    SizedBox(
                      width: 220.width,
                      height: 22.height,
                      child: Slider(
                        min: 0,
                        max: 9,
                        value: sliderValue.toDouble(),
                        activeColor: LiveColors.designStandardB1,
                        thumbColor: LiveColors.designStandardFlowkitWhite,
                        onChanged: (double value) {
                          _onSliderChanged(value);
                        },
                      ),
                    ),
                    SizedBox(width: 14.width),
                    Text(
                      sliderValue.toString(),
                      style: const TextStyle(
                          color: LiveColors.designStandardG6, fontSize: 12),
                    ),
                  ],
                ),
              );
            },
          );
        }
      },
    );
  }

  Widget _initBeautyListWidget() {
    return SizedBox(
      width: 1.screenWidth,
      height: 79.height,
      child: Padding(
        padding: EdgeInsets.symmetric(horizontal: 24.width),
        child: ListView.builder(
          shrinkWrap: true,
          physics: const AlwaysScrollableScrollPhysics(),
          scrollDirection: Axis.horizontal,
          itemCount: list.length,
          itemBuilder: (context, index) {
            return ValueListenableBuilder(
              valueListenable: selectIndex,
              builder: (context, selectIndex, _) {
                return GestureDetector(
                  onTap: () => _onTapIndex(index),
                  child: Container(
                    width: 56.width,
                    height: 79.height,
                    margin: EdgeInsets.only(right: 12.width),
                    child: Column(
                      children: [
                        Container(
                          width: 56.width,
                          height: 56.width,
                          decoration: BoxDecoration(
                            color: LiveColors.notStandardBlue30Transparency,
                            border: Border.all(
                                color: selectIndex == index
                                    ? LiveColors.designStandardB1
                                    : LiveColors.notStandardBlue30Transparency,
                                width: 2.width),
                            borderRadius: BorderRadius.circular(10.radius),
                          ),
                          child: Padding(
                            padding: EdgeInsets.all(13.radius),
                            child: Image.asset(
                              list[index].icon,
                              package: Constants.pluginName,
                            ),
                          ),
                        ),
                        SizedBox(height: 2.height),
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
            );
          },
        ),
      ),
    );
  }
}

extension on _BeautyPanelWidgetState {
  void _onSliderChanged(double value) {
    sliderValue.value = value.toInt();
    if (selectIndex.value == 1) {
      manager.setBeautyLevel(sliderValue.value);
    } else if (selectIndex.value == 2) {
      manager.setWhitenessLevel(sliderValue.value);
    } else if (selectIndex.value == 3) {
      manager.setRuddyLevel(sliderValue.value);
    }
  }

  String _getLeftTitle() {
    return list[selectIndex.value].title;
  }

  void _onTapIndex(int index) {
    selectIndex.value = index;
    final item = list[index];
    switch (item.type) {
      case BeautyItemType.none:
        sliderValue.value = 0;
        manager.closeBeautyEffect();
        break;
      case BeautyItemType.smooth:
        sliderValue.value = manager.state.smoothLevel.value;
        break;
      case BeautyItemType.whiteness:
        sliderValue.value = manager.state.whitenessLevel.value;
        break;
      case BeautyItemType.ruddy:
        sliderValue.value = manager.state.ruddyLevel.value;
        break;
      default:
        break;
    }
  }

  bool _isClose() {
    final state = manager.state;
    final value = state.smoothLevel.value +
        state.whitenessLevel.value +
        state.ruddyLevel.value;
    return value == 0;
  }

  void _initData() {
    BeautyState state =
        BeautyStateFactory.getState(BeautyManager.beautyStateKey);
    manager = BeautyManager(state: state);

    list = [
      BeautyItem(
          title: LiveKitLocalizations.of(Global.appContext())!
              .common_beauty_item_close,
          icon: LiveImages.selectNone,
          type: BeautyItemType.none),
      BeautyItem(
          title: LiveKitLocalizations.of(Global.appContext())!
              .common_beauty_item_smooth,
          icon: LiveImages.selectSmooth,
          type: BeautyItemType.smooth),
      BeautyItem(
          title: LiveKitLocalizations.of(Global.appContext())!
              .common_beauty_item_whiteness,
          icon: LiveImages.selectWhiteness,
          type: BeautyItemType.whiteness),
      BeautyItem(
          title: LiveKitLocalizations.of(Global.appContext())!
              .common_beauty_item_ruddy,
          icon: LiveImages.selectRuddy,
          type: BeautyItemType.ruddy),
    ];
    _onTapIndex(_isClose() ? 0 : 1);
  }
}

enum BeautyItemType { none, smooth, whiteness, ruddy }

class BeautyItem {
  String title;
  String icon;
  BeautyItemType type;

  BeautyItem({
    required this.title,
    required this.icon,
    required this.type,
  });
}
