import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

class BeautyPanelWidget extends BasicWidget {
  const BeautyPanelWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return BeautyPanelWidgetState();
  }
}

class BeautyPanelWidgetState extends BasicState<BeautyPanelWidget> {
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
      width: screenWidth,
      height: 350,
      decoration: const BoxDecoration(
        color: LivekitColors.livekitDesignStandardG2,
        borderRadius: BorderRadius.only(topLeft: Radius.circular(20), topRight: Radius.circular(20)),
      ),
      child: Column(children: [
        _initTitleWidget(),
        32.verticalSpace,
        _initSliderWidget(),
        16.verticalSpace,
        _initBeautyListWidget()
      ]),
    );
  }

  _initTitleWidget() {
    return SizedBox(
      height: 44,
      width: screenWidth,
      child: Stack(
        children: [
          Positioned(
            left: 14,
            child: GestureDetector(
              onTap: () {
                Navigator.pop(context);
              },
              child: Container(
                width: 44,
                height: 44,
                padding: const EdgeInsets.all(10),
                child: Image.asset(
                  LivekitImages.livekitReturnArrow,
                  package: Constants.pluginName,
                ),
              ),
            ),
          ),
          Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!.livekit_beauty_panel_title,
              style: const TextStyle(color: LivekitColors.livekitDesignStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  _initSliderWidget() {
    return ValueListenableBuilder(
      valueListenable: selectIndex,
      builder: (BuildContext context, int value, Widget? child) {
        if (selectIndex.value == 0) {
          return 22.verticalSpace;
        } else {
          return ValueListenableBuilder(
            valueListenable: sliderValue,
            builder: (BuildContext context, int value, Widget? child) {
              return SizedBox(
                height: 22,
                child: Row(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    Text(
                      _getLeftTitle(),
                      style: const TextStyle(color: LivekitColors.livekitDesignStandardG6, fontSize: 12),
                    ),
                    9.horizontalSpace,
                    SizedBox(
                      width: 220,
                      height: 22,
                      child: Slider(
                        min: 0,
                        max: 9,
                        value: sliderValue.value.toDouble(),
                        activeColor: LivekitColors.livekitDesignStandardB1,
                        thumbColor: LivekitColors.livekitDesignStandardFlowkitWhite,
                        onChanged: (double value) {
                          _onSliderChanged(value);
                        },
                      ),
                    ),
                    14.horizontalSpace,
                    Text(
                      sliderValue.value.toString(),
                      style: const TextStyle(color: LivekitColors.livekitDesignStandardG6, fontSize: 12),
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

  _initBeautyListWidget() {
    return SizedBox(
      width: screenWidth,
      height: 79,
      child: Padding(
        padding: const EdgeInsets.symmetric(horizontal: 24),
        child: ListView.builder(
          shrinkWrap: true,
          physics: const AlwaysScrollableScrollPhysics(),
          scrollDirection: Axis.horizontal,
          itemCount: list.length,
          itemBuilder: (context, index) {
            return ValueListenableBuilder(
              valueListenable: selectIndex,
              builder: (BuildContext context, int value, Widget? child) {
                return GestureDetector(
                  onTap: () => _onTapIndex(index),
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
                                color: selectIndex.value == index
                                    ? LivekitColors.livekitDesignStandardB1
                                    : LivekitColors.livekitNotStandardBlue30Transparency,
                                width: 2),
                            borderRadius: BorderRadius.circular(10),
                          ),
                          child: Padding(
                            padding: const EdgeInsets.all(10.0),
                            child: Image.asset(
                              list[index].icon,
                              package: Constants.pluginName,
                            ),
                          ),
                        ),
                        2.verticalSpace,
                        Text(
                          list[index].title,
                          style: const TextStyle(color: LivekitColors.livekitDesignStandardG6, fontSize: 12),
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

extension BeautyPanelWidgetStateLogicExtension on BeautyPanelWidgetState {
  void _onSliderChanged(double value) {
    sliderValue.value = value.toInt();
    if (selectIndex.value == 1) {
      liveController.getBeautyState().smoothLevel.value = sliderValue.value;
      liveController.liveService.setBeautyLevel(liveController.getBeautyState().smoothLevel.value);
    } else if (selectIndex.value == 2) {
      liveController.getBeautyState().whitenessLevel.value = sliderValue.value;
      liveController.liveService.setWhitenessLevel(liveController.getBeautyState().whitenessLevel.value);
    } else if (selectIndex.value == 3) {
      liveController.getBeautyState().ruddyLevel.value = sliderValue.value;
      liveController.liveService.setRuddyLevel(liveController.getBeautyState().ruddyLevel.value);
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
        liveController.getBeautyState().whitenessLevel.value = 0;
        liveController.getBeautyState().smoothLevel.value = 0;
        liveController.getBeautyState().ruddyLevel.value = 0;
        liveController.liveService.setBeautyLevel(liveController.getBeautyState().whitenessLevel.value);
        liveController.liveService.setWhitenessLevel(liveController.getBeautyState().smoothLevel.value);
        liveController.liveService.setRuddyLevel(liveController.getBeautyState().ruddyLevel.value);
        break;
      case BeautyItemType.smooth:
        sliderValue.value = liveController.getBeautyState().smoothLevel.value;
        break;
      case BeautyItemType.whiteness:
        sliderValue.value = liveController.getBeautyState().whitenessLevel.value;
        break;
      case BeautyItemType.ruddy:
        sliderValue.value = liveController.getBeautyState().ruddyLevel.value;
        break;
      default:
        break;
    }
  }

  bool _isClose() {
    final state = liveController.getBeautyState();
    final value = state.smoothLevel.value + state.whitenessLevel.value + state.ruddyLevel.value;
    return value == 0;
  }

  void _initData() {
    list = [
      BeautyItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_beauty_item_none,
          icon: LivekitImages.livekitSelectNone,
          type: BeautyItemType.none),
      BeautyItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_beauty_item_smooth,
          icon: LivekitImages.livekitSelectSmooth,
          type: BeautyItemType.smooth),
      BeautyItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_beauty_item_whiteness,
          icon: LivekitImages.livekitSelectWhiteness,
          type: BeautyItemType.whiteness),
      BeautyItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_beauty_item_ruddy,
          icon: LivekitImages.livekitSelectRuddy,
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
