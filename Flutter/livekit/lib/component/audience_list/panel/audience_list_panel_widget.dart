import 'package:flutter/material.dart';

import '../../../../../common/index.dart';
import '../manager/audience_list_manager.dart';

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
      height: 700.height,
      width: screenWidth,
      child: Column(
        children: [
          _initDiffWidget(15.height),
          _initTopWidget(),
          _initDiffWidget(12.height),
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
                  .common_anchor_audience_list_panel_title,
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
            height: 629.height,
            child: ListView.builder(
                itemCount: manager.state.audienceList.value.length,
                itemExtent: 60.height,
                itemBuilder: (BuildContext context, int index) {
                  final user = manager.state.audienceList.value[index];
                  return Padding(
                    padding: EdgeInsets.symmetric(horizontal: 20.width),
                    child: SizedBox(
                        height: 60.height,
                        width: double.infinity,
                        child: Stack(
                          children: [
                            Positioned(
                              left: 0,
                              top: 10.height,
                              child: Row(
                                children: [
                                  ClipRRect(
                                    borderRadius: BorderRadius.all(
                                        Radius.circular(20.radius)),
                                    child: SizedBox(
                                      width: 40.radius,
                                      height: 40.radius,
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
                                    width: 12.width,
                                    height: 44.height,
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
                              left: 52.width,
                              top: 59.height,
                              child: Container(
                                width: 251.width,
                                height: 1.height,
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
