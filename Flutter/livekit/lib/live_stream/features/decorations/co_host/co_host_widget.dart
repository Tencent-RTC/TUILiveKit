import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';

import '../../../../common/constants/constants.dart';
import '../../../../common/language/index.dart';
import '../../../../common/resources/index.dart';
import '../../../../common/widget/index.dart';
import '../../../manager/live_stream_manager.dart';

class CoHostWidget extends StatefulWidget {
  final CoHostUser coHostUser;
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;

  const CoHostWidget(
      {super.key,
      required this.coHostUser,
      required this.liveCoreController,
      required this.liveStreamManager});

  @override
  State<CoHostWidget> createState() => _CoHostWidgetState();
}

class _CoHostWidgetState extends State<CoHostWidget> {
  @override
  Widget build(BuildContext context) {
    return Stack(
      alignment: Alignment.center,
      children: [
        _buildMicAndNameWidget(),
        _buildAvatarWidget(),
        _buildConnectionStatusWidget()
      ],
    );
  }

  Widget _buildAvatarWidget() {
    return ValueListenableBuilder(
      valueListenable:
          widget.liveCoreController.userState.hasVideoStreamUserList,
      builder: (context, value, _) {
        return Visibility(
          visible: _isAvatarWidgetVisible(),
          child: Center(
            child: SizedBox(
              width: 45.width,
              height: 45.width,
              child: ClipOval(
                child: Image.network(
                  widget.coHostUser.connectionUser.avatarUrl,
                  errorBuilder: (context, error, stackTrace) {
                    return Image.asset(
                      LiveImages.defaultAvatar,
                      package: Constants.pluginName,
                    );
                  },
                ),
              ),
            ),
          ),
        );
      },
    );
  }

  Widget _buildMicAndNameWidget() {
    return Visibility(
      visible: _isUserNameWidgetVisible(),
      child: Positioned(
        left: 10.width,
        bottom: 4.height,
        child: Container(
          padding: EdgeInsets.only(
              left: 8.width, right: 8.width, top: 3.height, bottom: 3.height),
          decoration: BoxDecoration(
            color: LiveColors.userNameBlackColor,
            borderRadius: BorderRadius.circular(37.radius),
          ),
          child: Row(
            children: [
              ValueListenableBuilder(
                valueListenable:
                    widget.liveCoreController.userState.hasAudioStreamUserList,
                builder: (context, value, _) {
                  return Visibility(
                    visible: _isMicWidgetVisible(),
                    child: SizedBox(
                      width: 12.width,
                      height: 12.width,
                      child: Image.asset(
                        LiveImages.muteMicrophone,
                        package: Constants.pluginName,
                      ),
                    ),
                  );
                },
              ),
              SizedBox(
                width: 2.width,
              ),
              Text(
                (widget.coHostUser.connectionUser.userName.isNotEmpty)
                    ? widget.coHostUser.connectionUser.userName
                    : widget.coHostUser.connectionUser.userId,
                style: const TextStyle(
                    color: LiveColors.designStandardFlowkitWhite, fontSize: 10),
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
                  padding: EdgeInsets.only(top: 8.height, left: 8.width),
                  child: Container(
                    constraints: BoxConstraints(maxWidth: 60.width),
                    height: 28.height,
                    decoration: BoxDecoration(
                        borderRadius: BorderRadius.circular(12.height)),
                    child: Padding(
                      padding: EdgeInsets.only(
                          left: 4.width,
                          right: 4.width,
                          top: 5.height,
                          bottom: 5.height),
                      child: Text(
                        LiveKitLocalizations.of(Global.appContext())!
                            .common_battle_connecting,
                        style: const TextStyle(
                            fontSize: 12,
                            color: LiveColors.designStandardFlowkitWhite),
                      ),
                    ),
                  ),
                ));
          }),
    );
  }
}

extension on _CoHostWidgetState {
  bool _isAvatarWidgetVisible() {
    if (widget.liveCoreController.userState.hasVideoStreamUserList.value
        .contains(widget.coHostUser.connectionUser.userId) || widget.coHostUser.hasVideoStream) {
      return false;
    }
    return true;
  }

  bool _isUserNameWidgetVisible() {
    if (widget.coHostUser.connectionUser.userId ==
        widget.liveCoreController.userState.selfInfo.userId) {
      return false;
    }
    return true;
  }

  bool _isMicWidgetVisible() {
    if (widget.liveCoreController.userState.hasAudioStreamUserList.value
        .contains(widget.coHostUser.connectionUser.userId)) {
      return false;
    }
    return true;
  }

  bool _isConnectionStatusVisible() {
    if (widget.liveCoreController.battleState.isBattleStart.value &&
        !widget.liveCoreController.battleState.battlingUserList.value.any(
            (battleUser) =>
                battleUser.userId == widget.coHostUser.connectionUser.userId)) {
      return true;
    }
    return false;
  }
}
