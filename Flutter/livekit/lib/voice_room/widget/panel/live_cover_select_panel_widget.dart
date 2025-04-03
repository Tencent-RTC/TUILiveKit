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
      height: context.adapter.getHeight(700),
      width: screenWidth,
      child: Column(
        children: [
          SizedBox(
            height: context.adapter.getHeight(15),
          ),
          SizedBox(
            height: context.adapter.getHeight(44),
            width: screenWidth,
            child: Stack(
              children: [
                Positioned(
                  left: context.adapter.getWidth(14),
                  child: GestureDetector(
                    onTap: () {
                      Navigator.pop(context);
                    },
                    child: Container(
                      width: context.adapter.getWidth(44),
                      height: context.adapter.getWidth(44),
                      padding: EdgeInsets.all(context.adapter.getWidth(10)),
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
                        .live_preset_cover,
                    style: const TextStyle(
                        color: LiveColors.designStandardFlowkitWhite,
                        fontSize: 16),
                  ),
                ),
              ],
            ),
          ),
          Padding(
            padding: EdgeInsets.all(context.adapter.getWidth(15)),
            child: SizedBox(
              width: context.adapter.getWidth(345),
              height: context.adapter.getHeight(500),
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
                      padding: EdgeInsets.all(context.adapter.getWidth(5)),
                      child: ValueListenableBuilder(
                        valueListenable: selectedIndex,
                        builder:
                            (BuildContext context, int value, Widget? child) {
                          return Container(
                            width: context.adapter.getWidth(90),
                            height: context.adapter.getWidth(90),
                            decoration: BoxDecoration(
                                border: Border.all(
                                    color: selectedIndex.value == index
                                        ? LiveColors.designStandardB1
                                        : LiveColors.designStandardTransparent,
                                    width: context.adapter.getWidth(3)),
                                borderRadius:
                                BorderRadius.all(Radius.circular(context.adapter.getWidth(6)))),
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
            height: context.adapter.getHeight(20),
          ),
          GestureDetector(
            onTap: () {
              _setLiveCover();
            },
            child: Container(
              width: context.adapter.getWidth(200),
              height: context.adapter.getHeight(52),
              alignment: Alignment.center,
              decoration: BoxDecoration(
                borderRadius: BorderRadius.circular(context.adapter.getWidth(10)),
                color: LiveColors.designStandardB1,
              ),
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!
                    .live_set_as_cover,
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
