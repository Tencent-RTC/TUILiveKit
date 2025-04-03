import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/audience_list/manager/audience_list_manager.dart';

import '../../../../../common/index.dart';

class AudienceListPanelWidget extends StatefulWidget {
  final AudienceListManager manager;

  const AudienceListPanelWidget({super.key, required this.manager});

  @override
  State<AudienceListPanelWidget> createState() =>
      _AudienceListPanelWidgetState();
}

class _AudienceListPanelWidgetState extends State<AudienceListPanelWidget> {
  late final AudienceListManager manager;

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
          _initDiffWidget(context.adapter.getHeight(15)),
          _initTopWidget(),
          _initDiffWidget(context.adapter.getHeight(12)),
          _initListWidget(),
        ],
      ),
    );
  }
}

extension on _AudienceListPanelWidgetState {
  Widget _initDiffWidget(double height) {
    return SizedBox(
      height: height,
    );
  }

  Widget _initTopWidget() {
    final double screenWidth = MediaQuery.of(context).size.width;
    return SizedBox(
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
                  .live_anchor_audience_list_panel_title,
              style: const TextStyle(
                  color: LiveColors.designStandardFlowkitWhite, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  Widget _initListWidget() {
    final double screenWidth = MediaQuery.of(context).size.width;
    return ValueListenableBuilder(
      valueListenable: manager.state.audienceList,
      builder: (BuildContext context, value, Widget? child) {
        return SizedBox(
            width: screenWidth,
            height: context.adapter.getHeight(629),
            child: ListView.builder(
                itemCount: manager.state.audienceList.value.length,
                itemExtent: context.adapter.getHeight(60),
                itemBuilder: (BuildContext context, int index) {
                  final user = manager.state.audienceList.value[index];
                  return Padding(
                    padding: EdgeInsets.symmetric(
                        horizontal: context.adapter.getWidth(20)),
                    child: SizedBox(
                        height: context.adapter.getHeight(60),
                        width: double.infinity,
                        child: Stack(
                          children: [
                            Positioned(
                              left: 0,
                              top: context.adapter.getHeight(10),
                              child: Row(
                                children: [
                                  ClipRRect(
                                    borderRadius: BorderRadius.all(
                                        Radius.circular(
                                            context.adapter.getWidth(20))),
                                    child: SizedBox(
                                      width: context.adapter.getWidth(40),
                                      height: context.adapter.getWidth(40),
                                      child: Image.network(
                                        user.avatarUrl,
                                        fit: BoxFit.cover,
                                        errorBuilder:
                                            (context, error, stackTrace) {
                                          return Image.asset(
                                            LiveImages.defaultAvatar,
                                            package: Constants.pluginName,
                                          );
                                        },
                                      ),
                                    ),
                                  ),
                                  SizedBox(
                                    width: context.adapter.getWidth(12),
                                    height: context.adapter.getHeight(44),
                                  ),
                                  Text(
                                    user.userName.isNotEmpty
                                        ? user.userName
                                        : user.userId,
                                    style: const TextStyle(
                                        color: LiveColors
                                            .designStandardFlowkitWhite,
                                        fontSize: 16),
                                  )
                                ],
                              ),
                            ),
                            Positioned(
                              left: context.adapter.getWidth(52),
                              top: context.adapter.getHeight(59),
                              child: Container(
                                width: context.adapter.getWidth(251),
                                height: context.adapter.getHeight(1),
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
