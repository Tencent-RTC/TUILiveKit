import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../../../../../../common/index.dart';
import '../../../../../state/index.dart';
import '../../../video/index.dart';
import 'link_request_place_holder_widget.dart';

class AudienceVideoWidget extends BasicWidget {
  const AudienceVideoWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AudienceVideoWidgetState();
  }
}

class AudienceVideoWidgetState extends BasicState {
  final List<int> customLayoutId = [];
  final List<BasicWidget> videos = [];
  LinkRequestPlaceHolderWidget? linkRequestPlaceHolderWidget;

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: liveController.getViewState().linkStatus,
      builder: (BuildContext context, LinkStatus value, Widget? child) {
        _onLinkStatusChange();
        return ValueListenableBuilder(
          valueListenable: liveController.getSeatState().seatList,
          builder: (BuildContext context, List<SeatInfo> value, Widget? child) {
            return Container(
              color: LiveColors.black,
              child: _initVideoWidget(context),
            );
          },
        );
      },
    );
  }

  _initVideoWidget(BuildContext context) {
    customLayoutId.clear();
    videos.clear();
    _addAudienceWidget();
    _addSelfVideoWidget();
    return CustomMultiChildLayout(
      delegate: NineGridLayout(customLayoutId,
          center: Offset(MediaQuery.sizeOf(context).width / 2, MediaQuery.sizeOf(context).height / 2)),
      children: <Widget>[
        for (var id in customLayoutId) LayoutId(id: id, child: videos[id]),
      ],
    );
  }

  void _addAudienceWidget() {
    for (SeatInfo seatInfo in liveController.getSeatState().seatList.value) {
      customLayoutId.add(customLayoutId.length);
      VideoItemWidget? widget = liveController.getVideoViewFactory().createVideoWidget(seatInfo);
      if (widget != null) {
        videos.add(widget);
      }
    }
  }

  void _addSelfVideoWidget() {
    if (linkRequestPlaceHolderWidget == null) {
      return;
    }
    if (liveController.getUserState().selfInfo.role.value == TUIRole.generalUser) {
      if (liveController.getViewState().linkStatus.value == LinkStatus.applying) {
        customLayoutId.add(customLayoutId.length);
        videos.add(linkRequestPlaceHolderWidget!);
      }
    }
  }
}

extension AudienceVideoWidgetStateLogicExtension on AudienceVideoWidgetState {
  _onLinkStatusChange() {
    if (liveController.getUserState().selfInfo.role.value == TUIRole.generalUser) {
      if (liveController.getViewState().linkStatus.value == LinkStatus.applying) {
        linkRequestPlaceHolderWidget = LinkRequestPlaceHolderWidget(liveController: liveController);
      } else if (linkRequestPlaceHolderWidget != null) {
        customLayoutId.removeAt(customLayoutId.length - 1);
        videos.removeAt(customLayoutId.length - 1);
        linkRequestPlaceHolderWidget = null;
      }
    }
  }
}
