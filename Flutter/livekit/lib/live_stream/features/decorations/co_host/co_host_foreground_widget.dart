import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/constants/index.dart';
import 'package:tencent_live_uikit/common/resources/colors.dart';
import 'package:tencent_live_uikit/common/resources/images.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';

import '../../../../common/language/index.dart';
import '../../../../common/widget/index.dart';

class CoHostForegroundWidget extends StatefulWidget {
  final SeatFullInfo userInfo;
  final LiveCoreController liveCoreController;
  final ValueListenable<bool> isFloatWindowMode;

  const CoHostForegroundWidget({
    super.key,
    required this.userInfo,
    required this.liveCoreController,
    required this.isFloatWindowMode,
  });

  @override
  State<CoHostForegroundWidget> createState() => _CoHostForegroundWidgetState();
}

class _CoHostForegroundWidgetState extends State<CoHostForegroundWidget> {
  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
        valueListenable: widget.isFloatWindowMode,
        builder: (context, isFloatWindowMode, child) {
          return Visibility(
            visible: !isFloatWindowMode,
            child: Stack(
              alignment: Alignment.center,
              children: [
                _buildConnectionStatusWidget(),
                _buildMicAndNameWidget(),
              ],
            ),
          );
        });
  }

  Widget _buildMicAndNameWidget() {
    return Visibility(
      visible: widget.liveCoreController.coHostState.connectedUserList.value.length > 1,
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

  Widget _buildConnectionStatusWidget() {
    return Align(
      alignment: Alignment.topLeft,
      child: ListenableBuilder(
          listenable: Listenable.merge([
            widget.liveCoreController.battleState.isBattleStart,
            widget.liveCoreController.battleState.battlingUserList
          ]),
          builder: (context, _) {
            return Visibility(
                visible: _isConnectionStatusVisible(),
                child: Padding(
                  padding: EdgeInsets.only(left: 8.width, right: 8.width, top: 3.height, bottom: 3.height),
                  child: Container(
                    decoration: BoxDecoration(
                        borderRadius: BorderRadius.circular(37.radius), color: LiveColors.userNameBlackColor),
                    child: Padding(
                      padding: EdgeInsets.only(left: 4.width, right: 4.width, top: 5.height, bottom: 5.height),
                      child: Text(
                        LiveKitLocalizations.of(Global.appContext())!.common_battle_connecting,
                        style: const TextStyle(fontSize: 12, color: LiveColors.designStandardFlowkitWhite),
                      ),
                    ),
                  ),
                ));
          }),
    );
  }
}

extension on _CoHostForegroundWidgetState {
  bool _isConnectionStatusVisible() {
    if (widget.liveCoreController.battleState.isBattleStart.value &&
        !widget.liveCoreController.battleState.battlingUserList.value
            .any((battleUser) => battleUser.userId == widget.userInfo.userId)) {
      return true;
    }
    return false;
  }
}
