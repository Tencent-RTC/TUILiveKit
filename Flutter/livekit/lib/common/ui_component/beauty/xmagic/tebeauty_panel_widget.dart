import 'package:flutter/material.dart';
import 'package:tencent_effect_flutter/api/tencent_effect_api.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/common/ui_component/beauty/xmagic/xmagic_manager.dart';

class TEBeautyPanelWidget extends BasicWidget {
  const TEBeautyPanelWidget({super.key, required super.liveController});

  @override
  TEBeautyPanelWidgetState getState() {
    return TEBeautyPanelWidgetState();
  }
}

class TEBeautyPanelWidgetState extends BasicState<TEBeautyPanelWidget> {
  late final ValueNotifier<int> selectIndex = ValueNotifier(0);
  late final ValueNotifier<int> sliderValue = ValueNotifier(0);
  late final List<TEBeautyItem> list;

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
          const Center(
            child: Text(
              "XMagic Beauty",
              style: TextStyle(color: LivekitColors.livekitDesignStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
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

extension TebeautyPanelWidgetStateLogicExtension on TEBeautyPanelWidgetState {

  void _onTapIndex(int index) async {
    selectIndex.value = index;
    final item = list[index];
    switch (item.type) {
      case BeautyItemType.none:
        if (liveController.getBeautyState().enableBeauty.value) {
          liveController.getBeautyState().enableBeauty.value = false;
          liveController.liveService.enableCustomVideoProcess(false);
        }
        liveController.getBeautyState().xmagicType.value = 0;
        break;
      case BeautyItemType.keaituya:
        if (!liveController.getBeautyState().enableBeauty.value) {
          liveController.getBeautyState().enableBeauty.value = true;
          await liveController.liveService.enableCustomVideoProcess(true);
        }
        liveController.getBeautyState().xmagicType.value = 1;
        TencentEffectApi.getApi()?.setEffect("motion",
            0, XmagicManager.getResPath(BeautyItemType.keaituya), {});
        break;
      case BeautyItemType.kaixueqianhou:
        if (!liveController.getBeautyState().enableBeauty.value) {
          liveController.getBeautyState().enableBeauty.value = true;
          await liveController.liveService.enableCustomVideoProcess(true);
        }
        liveController.getBeautyState().xmagicType.value = 2;
        TencentEffectApi.getApi()?.setEffect("motion",
            0, XmagicManager.getResPath(BeautyItemType.kaixueqianhou), {});
        break;
      case BeautyItemType.xuanmeizhuang:
        if (!liveController.getBeautyState().enableBeauty.value) {
          liveController.getBeautyState().enableBeauty.value = true;
          await liveController.liveService.enableCustomVideoProcess(true);
        }
        liveController.getBeautyState().xmagicType.value = 3;
        TencentEffectApi.getApi()?.setEffect("motion",
            0, XmagicManager.getResPath(BeautyItemType.xuanmeizhuang), {});
        break;
      default:
        break;
    }
  }

  void _initData() {
    list = [
      TEBeautyItem(
          title: LiveKitLocalizations.of(Global.appContext())!.livekit_beauty_item_none,
          icon: LivekitImages.livekitSelectNone,
          type: BeautyItemType.none),
      TEBeautyItem(
          title: "特效1",
          icon: LivekitImages.livekitSelectSmooth,
          type: BeautyItemType.keaituya),
      TEBeautyItem(
          title: "特效2",
          icon: LivekitImages.livekitSelectWhiteness,
          type: BeautyItemType.kaixueqianhou),
      TEBeautyItem(
          title: "特效3",
          icon: LivekitImages.livekitSelectRuddy,
          type: BeautyItemType.xuanmeizhuang),
    ];
    _onTapIndex(liveController.getBeautyState().xmagicType.value);
  }
}

enum BeautyItemType { none, keaituya, kaixueqianhou, xuanmeizhuang }

class TEBeautyItem {
  String title;
  String icon;
  BeautyItemType type;

  TEBeautyItem({
    required this.title,
    required this.icon,
    required this.type,
  });
}
