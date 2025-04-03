import 'package:flutter/material.dart';

import '../../common/index.dart';

class AudienceListPanelWidget extends BasicWidget {
  const AudienceListPanelWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AudienceListPanelWidgetState();
  }
}

class AudienceListPanelWidgetState extends BasicState<AudienceListPanelWidget> {
  @override
  Widget build(BuildContext context) {
    final double screenWidth = MediaQuery.of(context).size.width;
    return SizedBox(
      height: 700,
      width: screenWidth,
      child: Column(
        children: [
          _initDiffWidget(15),
          _initTopWidget(),
          _initDiffWidget(12),
          _initListWidget(),
        ],
      ),
    );
  }
}

extension AudienceListPanelWidgetStateLogicExtension on AudienceListPanelWidgetState {
  Widget _initDiffWidget(double height) {
    return SizedBox(
      height: height,
    );
  }

  Widget _initTopWidget() {
    final double screenWidth = MediaQuery.of(context).size.width;
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
                  LiveImages.returnArrow,
                  package: Constants.pluginName,
                ),
              ),
            ),
          ),
          Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!.live_anchor_audience_list_panel_title,
              style: const TextStyle(color: LiveColors.designStandardFlowkitWhite, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  Widget _initListWidget() {
    final double screenWidth = MediaQuery.of(context).size.width;
    return ValueListenableBuilder(
      valueListenable: liveController.getUserState().userList,
      builder: (BuildContext context, value, Widget? child) {
        return SizedBox(
            width: screenWidth,
            height: 700 - 15 - 44 - 12,
            child: ListView.builder(
                itemCount: liveController.getUserState().userList.value.length,
                itemExtent: 60.0,
                itemBuilder: (BuildContext context, int index) {
                  final user = liveController.getUserState().userList.value.toList()[index];
                  return Padding(
                    padding: const EdgeInsets.symmetric(horizontal: 24),
                    child: SizedBox(
                        height: 60,
                        width: double.infinity,
                        child: Stack(
                          children: [
                            Positioned(
                              left: 0,
                              top: 10,
                              child: Row(
                                children: [
                                  ClipRRect(
                                    borderRadius: const BorderRadius.all(Radius.circular(20)),
                                    child: SizedBox(
                                      width: 40,
                                      height: 40,
                                      child: Image.network(
                                        user.avatarUrl.value ?? "",
                                        fit: BoxFit.cover,
                                        errorBuilder: (context, error, stackTrace) {
                                          return Image.asset(
                                            LiveImages.defaultAvatar,
                                            package: Constants.pluginName,
                                          );
                                        },
                                      ),
                                    ),
                                  ),
                                  const SizedBox(
                                    width: 12,
                                    height: 44,
                                  ),
                                  Text(
                                    user.name.value ?? "",
                                    style: const TextStyle(
                                        color: LiveColors.designStandardFlowkitWhite, fontSize: 16),
                                  )
                                ],
                              ),
                            ),
                            Positioned(
                              left: 52,
                              top: 59,
                              child: Container(
                                width: screenWidth - 76 - 24 * 2,
                                height: 1,
                                color: LiveColors.notStandardBlue30Transparency,
                              ),
                            )
                          ],
                        )),
                  );
                }));
      },
    );
  }
}
