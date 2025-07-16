import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/constants/index.dart';
import 'package:tencent_live_uikit/common/resources/colors.dart';
import 'package:tencent_live_uikit/common/resources/images.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';

class CoGuestWidget extends StatefulWidget {
  final TUIUserInfo userInfo;
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;

  const CoGuestWidget({
    super.key,
    required this.userInfo,
    required this.liveCoreController,
    required this.liveStreamManager,
  });

  @override
  State<CoGuestWidget> createState() => _CoGuestWidgetState();
}

class _CoGuestWidgetState extends State<CoGuestWidget> {
  @override
  Widget build(BuildContext context) {
    return Stack(
      alignment: Alignment.center,
      children: [
        _buildMicAndNameWidget(),
        _buildAvatarWidget(),
      ],
    );
  }

  _buildAvatarWidget() {
    return ValueListenableBuilder(
      valueListenable:
          widget.liveCoreController.userState.hasVideoStreamUserList,
      builder: (BuildContext context, value, Widget? child) {
        return Visibility(
          visible: _isAvatarWidgetVisible(),
          child: Center(
            child: SizedBox(
              width: 45.width,
              height: 45.width,
              child: ClipOval(
                child: Image.network(
                  widget.userInfo.avatarUrl,
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

  _buildMicAndNameWidget() {
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
                builder: (BuildContext context, value, Widget? child) {
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
                (widget.userInfo.userName.isNotEmpty)
                    ? widget.userInfo.userName
                    : widget.userInfo.userId,
                style: const TextStyle(
                    color: LiveColors.designStandardFlowkitWhite, fontSize: 10),
              )
            ],
          ),
        ),
      ),
    );
  }
}

extension on _CoGuestWidgetState {
  bool _isAvatarWidgetVisible() {
    final hasVideoStream = widget.userInfo.hasVideoStream ?? false;
    if (widget.liveCoreController.userState.hasVideoStreamUserList.value
            .contains(widget.userInfo.userId) ||
        hasVideoStream) {
      return false;
    }
    return true;
  }

  bool _isUserNameWidgetVisible() {
    if (widget.userInfo.userId ==
        widget.liveCoreController.userState.selfInfo.userId) {
      return false;
    }
    return true;
  }

  bool _isMicWidgetVisible() {
    if (widget.liveCoreController.userState.hasAudioStreamUserList.value
        .contains(widget.userInfo.userId)) {
      return false;
    }
    return true;
  }
}
