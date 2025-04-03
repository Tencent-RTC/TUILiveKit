import 'package:flutter/material.dart';

import '../../common/index.dart';

class LiveCoverSelectWidget extends BasicWidget {
  const LiveCoverSelectWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return LiveCoverSelectWidgetState();
  }
}

class LiveCoverSelectWidgetState extends BasicState<LiveCoverSelectWidget> {
  ValueNotifier<int> selectedIndex = ValueNotifier(0);

  @override
  Widget build(BuildContext context) {
    final double screenWidth = MediaQuery.of(context).size.width;
    return SizedBox(
      height: 700,
      width: screenWidth,
      child: Column(
        children: [
          const SizedBox(
            height: 15,
          ),
          SizedBox(
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
                        LiveImages.returnArrow,
                        package: Constants.pluginName,
                      ),
                    ),
                  ),
                ),
                Center(
                  child: Text(
                    LiveKitLocalizations.of(Global.appContext())!.live_preset_cover,
                    style: const TextStyle(color: LiveColors.designStandardFlowkitWhite, fontSize: 16),
                  ),
                ),
              ],
            ),
          ),
          Padding(
            padding: const EdgeInsets.all(15.0),
            child: SizedBox(
              width: screenWidth - 30,
              height: 500,
              child: GridView.builder(
                itemCount: Constants.coverUrlList.length,
                gridDelegate: const SliverGridDelegateWithFixedCrossAxisCount(crossAxisCount: 3, childAspectRatio: 1.0),
                itemBuilder: (BuildContext context, int index) {
                  return GestureDetector(
                    onTap: () {
                      _onLiveCoverItemSelected(index);
                    },
                    child: Padding(
                      padding: const EdgeInsets.all(5),
                      child: ValueListenableBuilder(
                        valueListenable: selectedIndex,
                        builder: (BuildContext context, int value, Widget? child) {
                          return Container(
                            width: 90,
                            height: 90,
                            decoration: BoxDecoration(
                              color: selectedIndex.value == index
                                  ? LiveColors.designStandardB1
                                  : LiveColors.designStandardFlowkitWhite,
                            ),
                            padding: const EdgeInsets.all(5),
                            child: Image.network(
                              Constants.coverUrlList[index],
                              fit: BoxFit.cover,
                              errorBuilder: (context, error, stackTrace) {
                                return Image.asset(
                                  LiveImages.streamDefaultCover,
                                  package: Constants.pluginName,
                                );
                              },
                            ),
                          );
                        },
                      ),
                    ),
                  );
                },
              ),
            ),
          ),
          const SizedBox(
            height: 20,
          ),
          GestureDetector(
            onTap: () {
              _setLiveCover();
            },
            child: Container(
              width: 200,
              height: 52,
              alignment: Alignment.center,
              decoration: BoxDecoration(
                borderRadius: BorderRadius.circular(10),
                color: LiveColors.designStandardB1,
              ),
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!.live_set_as_cover,
                style: const TextStyle(
                    color: LiveColors.designStandardFlowkitWhite, fontSize: 16, fontWeight: FontWeight.w700),
              ),
            ),
          ),
        ],
      ),
    );
  }
}

extension LiveCoverSelectWidgetStateLogicExtension on LiveCoverSelectWidgetState {
  _onLiveCoverItemSelected(int index) {
    selectedIndex.value = index;
  }

  _setLiveCover() {
    liveController.getRoomSate().coverURL.value = Constants.coverUrlList[selectedIndex.value];
    Navigator.of(Global.appContext()).pop();
  }
}
