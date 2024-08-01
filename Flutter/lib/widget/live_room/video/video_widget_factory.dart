import 'dart:collection';

import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/manager/index.dart';
import 'package:tencent_live_uikit/state/operation/seat_state.dart';
import 'package:tencent_live_uikit/widget/live_room/video/index.dart';

class VideoWidgetFactory {
  final LiveController _liveController;
  final Map<String, VideoItemWidget> _videoWidgetMap = HashMap<String, VideoItemWidget>();
  final Map<String, GlobalKey> _globalKeyMap = HashMap<String, GlobalKey>();

  VideoWidgetFactory({required LiveController liveController}) : _liveController = liveController;

  VideoItemWidget? createVideoWidget(SeatInfo seatInfo) {
    if (seatInfo.userId.value.isEmpty) {
      LiveKitLogger.error('createVideoWidget seatInfo: $seatInfo');
      return null;
    }
    GlobalKey? globalKey = _findGlobalKey(seatInfo.userId.value);
    VideoItemWidget? videoWidget = _findVideoWidget(seatInfo.userId.value);
    if (videoWidget != null) {
      return videoWidget;
    }
    videoWidget = VideoItemWidget(
      key: globalKey,
      liveController: _liveController,
      seatInfo: seatInfo,
    );
    _videoWidgetMap[seatInfo.userId.value] = videoWidget;
    return videoWidget;
  }

  void removeVideoWidgetByUserId(String userId) {
    _videoWidgetMap.remove(userId);
  }

  void clear() {
    _videoWidgetMap.clear();
    _globalKeyMap.clear();
  }

  VideoItemWidget? _findVideoWidget(String userId) {
    if (userId.isEmpty) {
      return null;
    }
    if (_videoWidgetMap.containsKey(userId)) {
      return _videoWidgetMap[userId];
    }
    return null;
  }

  GlobalKey? _findGlobalKey(String userId) {
    if (userId.isEmpty) {
      return null;
    }
    if (_globalKeyMap.containsKey(userId)) {
      return _globalKeyMap[userId];
    } else {
      GlobalKey globalKey = GlobalKey();
      _globalKeyMap[userId] = globalKey;
      return globalKey;
    }
  }
}
