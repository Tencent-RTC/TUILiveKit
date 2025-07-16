import 'package:flutter/material.dart';

import '../../../common/screen/index.dart';
import '../../../common/constants/constants.dart';
import '../../../common/resources/index.dart';
import '../../../component/audience_list/index.dart';
import '../../../component/live_info/index.dart';
import '../../index.dart';

enum TopWidgetTapEvent { stop, audienceList, liveInfo }

class TopWidget extends StatefulWidget {
  final VoiceRoomManager manager;
  final void Function(TopWidgetTapEvent event)? onTapTopWidget;

  const TopWidget({super.key, required this.manager, this.onTapTopWidget});

  @override
  State<TopWidget> createState() => _TopWidgetState();
}

class _TopWidgetState extends State<TopWidget> {
  late final VoiceRoomManager manager;

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
  }

  @override
  Widget build(BuildContext context) {
    return Stack(children: [
      _initLiveInfoWidget(),
      _initAudienceListWidget(),
      _initLeaveButton()
    ]);
  }

  Widget _initLiveInfoWidget() {
    return GestureDetector(
      onTap: () {
        widget.onTapTopWidget?.call(TopWidgetTapEvent.liveInfo);
      },
      child: Container(
          constraints:
              BoxConstraints(maxHeight: 40.height, maxWidth: 200.width),
          height: 40.height,
          child: LiveInfoWidget(roomId: manager.roomState.roomId)),
    );
  }

  Widget _initAudienceListWidget() {
    return Positioned(
        top: 8.height,
        bottom: 8.height,
        right: 30.width,
        child: GestureDetector(
          onTap: () {
            widget.onTapTopWidget?.call(TopWidgetTapEvent.audienceList);
          },
          child: Container(
              constraints: BoxConstraints(maxWidth: 107.width),
              child: AudienceListWidget(roomId: manager.roomState.roomId)),
        ));
  }

  Widget _initLeaveButton() {
    final isOwner =
        manager.roomState.ownerInfo.userId == manager.userState.selfInfo.userId;
    return Positioned(
        top: 10.height,
        bottom: 10.height,
        right: 0,
        child: SizedBox(
            width: 20.radius,
            height: 20.radius,
            child: GestureDetector(
              onTap: () => widget.onTapTopWidget?.call(TopWidgetTapEvent.stop),
              child: Image.asset(
                isOwner ? LiveImages.close : LiveImages.audienceClose,
                package: Constants.pluginName,
              ),
            )));
  }
}
