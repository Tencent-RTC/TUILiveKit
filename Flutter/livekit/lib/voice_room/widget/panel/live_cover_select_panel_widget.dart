import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';

import '../../../common/index.dart';

class LiveCoverSelectPanelWidget extends StatefulWidget {
  final VoiceRoomManager manager;
  final List<String> coverUrls;
  final String initialCoverUrl;

  const LiveCoverSelectPanelWidget(
      {super.key,
      required this.manager,
      required this.coverUrls,
      required this.initialCoverUrl});

  @override
  State<LiveCoverSelectPanelWidget> createState() =>
      _LiveCoverSelectPanelWidgetState();
}

class _LiveCoverSelectPanelWidgetState
    extends State<LiveCoverSelectPanelWidget> {
  late final VoiceRoomManager manager;
  ValueNotifier<int> selectedIndex = ValueNotifier(0);

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
  }

  @override
  Widget build(BuildContext context) {
    final double screenWidth = MediaQuery.of(context).size.width;
    return SizedBox(
      height: 700.height,
      width: screenWidth,
      child: Column(
        children: [
          SizedBox(
            height: 15.height,
          ),
          SizedBox(
            height: 44.height,
            width: screenWidth,
            child: Stack(
              children: [
                Positioned(
                  left: 14.width,
                  child: GestureDetector(
                    onTap: () {
                      Navigator.pop(context);
                    },
                    child: Container(
                      width: 44.radius,
                      height: 44.radius,
                      padding: EdgeInsets.all(10.radius),
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
                        .common_title_preset_cover,
                    style: const TextStyle(
                        color: LiveColors.designStandardFlowkitWhite,
                        fontSize: 16),
                  ),
                ),
              ],
            ),
          ),
          Padding(
            padding: EdgeInsets.all(15.radius),
            child: SizedBox(
              width: 345.width,
              height: 500.height,
              child: GridView.builder(
                itemCount: widget.coverUrls.length,
                gridDelegate: const SliverGridDelegateWithFixedCrossAxisCount(
                    crossAxisCount: 3, childAspectRatio: 1.0),
                itemBuilder: (BuildContext context, int index) {
                  return GestureDetector(
                    onTap: () {
                      _onLiveCoverItemSelected(index);
                    },
                    child: Padding(
                      padding: EdgeInsets.all(5.radius),
                      child: ValueListenableBuilder(
                        valueListenable: selectedIndex,
                        builder:
                            (BuildContext context, int value, Widget? child) {
                          return Container(
                            width: 90.radius,
                            height: 90.radius,
                            decoration: BoxDecoration(
                                border: Border.all(
                                    color: selectedIndex.value == index
                                        ? LiveColors.designStandardB1
                                        : LiveColors.designStandardTransparent,
                                    width: 3.width),
                                borderRadius: BorderRadius.all(
                                    Radius.circular(6.radius))),
                            child: Image.network(
                              widget.coverUrls[index],
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
          SizedBox(
            height: 20.height,
          ),
          GestureDetector(
            onTap: () {
              _setLiveCover();
            },
            child: Container(
              width: 200.width,
              height: 52.height,
              alignment: Alignment.center,
              decoration: BoxDecoration(
                borderRadius: BorderRadius.circular(10.radius),
                color: LiveColors.designStandardB1,
              ),
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!
                    .common_set_as_cover,
                style: const TextStyle(
                    color: LiveColors.designStandardFlowkitWhite,
                    fontSize: 16,
                    fontWeight: FontWeight.w700),
              ),
            ),
          ),
        ],
      ),
    );
  }
}

extension on _LiveCoverSelectPanelWidgetState {
  void _onLiveCoverItemSelected(int index) {
    selectedIndex.value = index;
  }

  void _setLiveCover() {
    manager.onCoverUrlChanged(widget.coverUrls[selectedIndex.value]);
    Navigator.of(Global.appContext()).pop();
  }
}
