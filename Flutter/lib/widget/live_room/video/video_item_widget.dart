import 'dart:collection';

import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/common/tui_video_view.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/state/index.dart';

class VideoItemWidget extends BasicWidget {
  const VideoItemWidget({super.key, required super.liveController, required this.seatInfo});

  final SeatInfo seatInfo;

  @override
  VideoItemState getState() {
    return VideoItemState();
  }
}

class VideoItemState extends BasicState<VideoItemWidget> {
  int _nativeViewPtr = 0;

  @override
  Widget build(BuildContext context) {
    return Container(
      color: LivekitColors.livekitDesignStandardG2,
      child: Stack(
        children: [_initVideoView(), _initAvatarWidget(), _initNameWidget()],
      ),
    );
  }

  _initVideoView() {
    return VideoView(
      onViewCreated: (id) {
        _nativeViewPtr = id;
        if (widget.seatInfo.userId.value == TUIRoomEngine.getSelfInfo().userId) {
          liveController.mediaController.setLocalVideoView(_nativeViewPtr);
        } else {
          liveController.mediaController
              .setRemoteVideoView(widget.seatInfo.userId.value, TUIVideoStreamType.cameraStream, _nativeViewPtr);
          liveController.mediaController
              .startPlayRemoteVideo(widget.seatInfo.userId.value, TUIVideoStreamType.cameraStream, null);
        }
      },
    );
  }

  _initAvatarWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getUserState().hasVideoStreamUserList,
      builder: (BuildContext context, LinkedHashSet<String> value, Widget? child) {
        return Visibility(
          visible: _avatarIsVisible(),
          child: Positioned.fill(
            child: Align(
              alignment: AlignmentDirectional.center,
              child: SizedBox(
                width: 50,
                height: 50,
                child: ValueListenableBuilder(
                    valueListenable: widget.seatInfo.avatarUrl,
                    builder: (BuildContext context, String? value, Widget? child) {
                      return ClipOval(
                          child: Image.network(
                        widget.seatInfo.avatarUrl.value!,
                        errorBuilder: (context, error, stackTrace) {
                          return Image.asset(
                            LivekitImages.livekitDefaultAvatar,
                            package: Constants.pluginName,
                          );
                        },
                      ));
                    }),
              ),
            ),
          ),
        );
      },
    );
  }

  _initNameWidget() {
    return Positioned(
      left: 10,
      bottom: 4,
      child: ValueListenableBuilder(
        valueListenable: liveController.getSeatState().seatList,
        builder: (BuildContext context, List<SeatInfo> value, Widget? child) {
          return ValueListenableBuilder(
            valueListenable: widget.seatInfo.name,
            builder: (BuildContext context, String? value, Widget? child) {
              return Visibility(
                visible: _nameIsVisible(),
                child: Container(
                  padding: const EdgeInsets.only(left: 4, right: 4, top: 1, bottom: 3),
                  decoration: BoxDecoration(
                    color: LivekitColors.livekitUserNameBlackColor,
                    borderRadius: BorderRadius.circular(18),
                  ),
                  child: Text(
                      (widget.seatInfo.name.value != null && widget.seatInfo.name.value!.isNotEmpty)
                          ? widget.seatInfo.name.value!
                          : widget.seatInfo.userId.value,
                      style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 10)),
                ),
              );
            },
          );
        },
      ),
    );
  }
}

extension VideoItemStateLogicExtension on VideoItemState {
  bool _nameIsVisible() {
    if (widget.seatInfo.userId.value == liveController.getUserState().selfInfo.userId) {
      return false;
    } else {
      if (liveController.getSeatState().seatList.value.length > 1) {
        return true;
      } else {
        return false;
      }
    }
  }

  bool _avatarIsVisible() {
    if (LiveStatus.previewing == liveController.getViewState().liveStatus.value) {
      return false;
    }
    if (liveController.getUserState().hasVideoStreamUserList.value.contains(widget.seatInfo.userId.value)) {
      return false;
    }
    return true;
  }
}
