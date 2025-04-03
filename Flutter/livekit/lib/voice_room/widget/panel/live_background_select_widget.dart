import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';

import '../../../common/index.dart';

class LiveBackgroundSelectPanelWidget extends StatefulWidget {
  final VoiceRoomManager manager;
  final List<String> backgroundUrls;
  final String initialBackgroundUrl;

  const LiveBackgroundSelectPanelWidget(
      {super.key,
      required this.manager,
      required this.backgroundUrls,
      required this.initialBackgroundUrl});

  @override
  State<LiveBackgroundSelectPanelWidget> createState() =>
      _LiveBackgroundSelectPanelWidgetState();
}

class _LiveBackgroundSelectPanelWidgetState
    extends State<LiveBackgroundSelectPanelWidget> {
  late final VoiceRoomManager manager;
  ValueNotifier<int> selectedIndex = ValueNotifier(0);

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
    final initialIndex =
        widget.backgroundUrls.indexOf(widget.initialBackgroundUrl);
    if (initialIndex != -1) {
      selectedIndex.value = initialIndex;
    }
  }

  @override
  Widget build(BuildContext context) {
    final double screenWidth = MediaQuery.of(context).size.width;
    return SizedBox(
      height: context.adapter.getHeight(483),
      width: screenWidth,
      child: Column(
        children: [
          SizedBox(height: context.adapter.getHeight(15)),
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
                      height: context.adapter.getHeight(14),
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
                        .live_settings_bg_image,
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
              height: context.adapter.getHeight(300),
              child: GridView.builder(
                itemCount: widget.backgroundUrls.length,
                gridDelegate: const SliverGridDelegateWithFixedCrossAxisCount(
                    crossAxisCount: 3, childAspectRatio: 1.0),
                itemBuilder: (BuildContext context, int index) {
                  return GestureDetector(
                    onTap: () {
                      _onLiveBackgroundItemSelected(index);
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
                            child: CachedNetworkImage(
                                imageUrl: widget.backgroundUrls[index],
                                fit: BoxFit.cover,
                                placeholder: (context, url) {
                                  return Image.asset(
                                    LiveImages.defaultBackground,
                                    package: Constants.pluginName,
                                  );
                                }),
                          );
                        },
                      ),
                    ),
                  );
                },
              ),
            ),
          ),
          SizedBox(height: context.adapter.getHeight(20)),
          GestureDetector(
            onTap: () {
              _setLiveBackground();
            },
            child: Container(
              width: context.adapter.getWidth(201),
              height: context.adapter.getHeight(40),
              alignment: Alignment.center,
              decoration: BoxDecoration(
                borderRadius:
                    BorderRadius.circular(context.adapter.getHeight(20)),
                color: LiveColors.designStandardB1,
              ),
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!
                    .live_set_as_background,
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

extension on _LiveBackgroundSelectPanelWidgetState {
  void _onLiveBackgroundItemSelected(int index) {
    selectedIndex.value = index;
  }

  void _setLiveBackground() {
    final backgroundUrl = widget.backgroundUrls[selectedIndex.value];
    manager.onBackgroundUrlChanged(backgroundUrl);
    if (manager.roomState.ownerInfo.userId.isNotEmpty) {
      final liveInfo = TUILiveInfo();
      liveInfo.roomInfo.roomId = manager.roomState.roomId;
      liveInfo.backgroundUrl = backgroundUrl;
      manager.setLiveInfo(liveInfo, [TUILiveModifyFlag.backgroundUrl]);
    }
    Navigator.of(Global.appContext()).pop();
  }
}
