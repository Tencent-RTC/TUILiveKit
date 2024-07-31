import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/state/index.dart';
import 'package:tencent_live_uikit/widget/live_room/video/index.dart';

class AnchorVideoWidget extends BasicWidget {
  const AnchorVideoWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AnchorVideoWidgetState();
  }
}

class AnchorVideoWidgetState extends BasicState<AnchorVideoWidget> {
  final List<int> customLayoutId = [];
  final List<VideoItemWidget> videos = [];

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: liveController.getSeatState().seatList,
      builder: (BuildContext context, List<SeatInfo> value, Widget? child) {
        return Container(
          color: LivekitColors.livekitBlack,
          child: _initVideoWidget(context),
        );
      },
    );
  }

  _initVideoWidget(BuildContext context) {
    customLayoutId.clear();
    videos.clear();
    liveController.mediaController.openLocalCamera();
    _addAnchorWidget();
    _addAudienceWidget();
    return CustomMultiChildLayout(
      delegate: NineGridLayout(customLayoutId,
          center: Offset(MediaQuery.sizeOf(context).width / 2, MediaQuery.sizeOf(context).height / 2)),
      children: <Widget>[
        for (var id in customLayoutId) LayoutId(id: id, child: videos[id]),
      ],
    );
  }

  void _addAnchorWidget() {
    SeatInfo seatInfo = SeatInfo();
    seatInfo.userId.value = TUIRoomEngine.getSelfInfo().userId;
    seatInfo.avatarUrl.value = TUIRoomEngine.getSelfInfo().avatarUrl;
    seatInfo.name.value = TUIRoomEngine.getSelfInfo().userName;
    customLayoutId.add(customLayoutId.length);
    VideoItemWidget? widget = liveController.getVideoViewFactory().createVideoWidget(seatInfo);
    if (widget != null) {
      videos.add(widget);
    }
  }

  void _addAudienceWidget() {
    for (SeatInfo seatInfo in liveController.getSeatState().seatList.value) {
      if (TUIRoomEngine.getSelfInfo().userId == seatInfo.userId.value) {
        continue;
      }
      customLayoutId.add(customLayoutId.length);
      VideoItemWidget? widget = liveController.getVideoViewFactory().createVideoWidget(seatInfo);
      if (widget != null) {
        videos.add(widget);
      }
    }
  }
}
