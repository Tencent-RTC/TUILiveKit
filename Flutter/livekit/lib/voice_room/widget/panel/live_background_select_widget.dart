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
    final double screenWidth = MediaQuery.sizeOf(context).width;
    return SizedBox(
      height: 483.height,
      width: screenWidth,
      child: Column(
        children: [
          SizedBox(height: 15.height),
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
                      width: 44.width,
                      height: 14.height,
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
                        .common_settings_bg_image,
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
              height: 300.height,
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
          SizedBox(height: 20.height),
          GestureDetector(
            onTap: () {
              _setLiveBackground();
            },
            child: Container(
              width: 201.width,
              height: 40.height,
              alignment: Alignment.center,
              decoration: BoxDecoration(
                borderRadius: BorderRadius.circular(20.radius),
                color: LiveColors.designStandardB1,
              ),
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!
                    .common_set_as_background,
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
      liveInfo.roomId = manager.roomState.roomId;
      liveInfo.backgroundUrl = backgroundUrl;
      manager.setLiveInfo(liveInfo, [TUILiveModifyFlag.backgroundUrl]);
    }
    Navigator.of(Global.appContext()).pop();
  }
}
