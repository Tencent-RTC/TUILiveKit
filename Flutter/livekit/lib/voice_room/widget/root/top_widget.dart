import 'package:flutter/material.dart';

import '../../../common/screen/index.dart';
import '../../../common/constants/constants.dart';
import '../../../common/resources/index.dart';
import '../../index.dart';
import '../component/index.dart';

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
          constraints: BoxConstraints(
              maxHeight: context.adapter.getHeight(40),
              maxWidth: context.adapter.getWidth(200)),
          height: context.adapter.getHeight(40),
          child: LiveInfoWidget(roomId: manager.roomState.roomId)),
    );
  }

  Widget _initAudienceListWidget() {
    return Positioned(
        top: context.adapter.getHeight(8),
        bottom: context.adapter.getHeight(8),
        right: context.adapter.getWidth(30),
        child: GestureDetector(
          onTap: () {
            widget.onTapTopWidget?.call(TopWidgetTapEvent.audienceList);
          },
          child: Container(constraints: BoxConstraints(maxWidth: context.adapter.getWidth(107))
          ,child: AudienceListWidget(roomId: manager.roomState.roomId)),
        ));
  }

  Widget _initLeaveButton() {
    final isOwner =
        manager.roomState.ownerInfo.userId == manager.userState.selfInfo.userId;
    return Positioned(
        top: context.adapter.getHeight(10),
        bottom: context.adapter.getHeight(10),
        right: 0,
        child: SizedBox(
            width: context.adapter.getWidth(20),
            height: context.adapter.getWidth(20),
            child: GestureDetector(
              onTap: () => widget.onTapTopWidget?.call(TopWidgetTapEvent.stop),
              child: Image.asset(
                isOwner
                    ? LiveImages.close
                    : LiveImages.audienceClose,
                package: Constants.pluginName,
              ),
            )));
  }
}
