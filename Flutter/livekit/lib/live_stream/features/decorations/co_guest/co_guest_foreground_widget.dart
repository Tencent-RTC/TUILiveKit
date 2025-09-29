import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/constants/index.dart';
import 'package:tencent_live_uikit/common/resources/colors.dart';
import 'package:tencent_live_uikit/common/resources/images.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';

class CoGuestForegroundWidget extends StatefulWidget {
  final SeatFullInfo userInfo;
  final LiveCoreController liveCoreController;

  const CoGuestForegroundWidget({
    super.key,
    required this.userInfo,
    required this.liveCoreController,
  });

  @override
  State<CoGuestForegroundWidget> createState() => _CoGuestWidgetState();
}

class _CoGuestWidgetState extends State<CoGuestForegroundWidget> {
  @override
  Widget build(BuildContext context) {
    return Stack(
      alignment: Alignment.center,
      children: [
        _buildMicAndNameWidget(),
      ],
    );
  }

  _buildMicAndNameWidget() {
    return Visibility(
      visible: widget.liveCoreController.coGuestState.connectedUserList.value.length > 1,
      child: Positioned(
        left: 10.width,
        bottom: 4.height,
        child: Container(
          padding: EdgeInsets.only(left: 8.width, right: 8.width, top: 3.height, bottom: 3.height),
          decoration: BoxDecoration(
            color: LiveColors.userNameBlackColor,
            borderRadius: BorderRadius.circular(37.radius),
          ),
          child: Row(
            children: [
              Visibility(
                visible: widget.userInfo.userMicrophoneStatus != DeviceStatus.opened,
                child: SizedBox(
                  width: 12.width,
                  height: 12.width,
                  child: Image.asset(
                    LiveImages.muteMicrophone,
                    package: Constants.pluginName,
                  ),
                ),
              ),
              SizedBox(
                width: 2.width,
              ),
              Text(
                (widget.userInfo.userName.isNotEmpty) ? widget.userInfo.userName : widget.userInfo.userId,
                style: const TextStyle(color: LiveColors.designStandardFlowkitWhite, fontSize: 10),
              )
            ],
          ),
        ),
      ),
    );
  }
}
