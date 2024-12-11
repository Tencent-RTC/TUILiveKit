import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

import 'index.dart';

class TUILiveKitNavigatorObserver extends NavigatorObserver {
  static final TUILiveKitNavigatorObserver instance = TUILiveKitNavigatorObserver._internal();

  factory TUILiveKitNavigatorObserver() {
    return instance;
  }

  static const String routeLiveRoomAudience = "route_live_room_audience";

  static bool isRepeatClick = false;

  TUILiveKitNavigatorObserver._internal() {
    LiveKitLogger.info('TUICallKitNavigatorObserver Init');
    Boot.instance;
  }

  BuildContext getContext() {
    return navigator!.context;
  }

  enterLiveRoomAudiencePage(String roomId) {
    if (isRepeatClick) {
      return;
    }
    Navigator.push(
        getContext(),
        MaterialPageRoute(
          settings: const RouteSettings(name: routeLiveRoomAudience),
          builder: (context) {
            return TUILiveRoomAudienceWidget(roomId: roomId);
          },
        ));
    isRepeatClick = false;
  }

  backToLiveRoomAudiencePage() {
    Navigator.popUntil(getContext(), (route) {
      if (route.settings.name == routeLiveRoomAudience) {
        return true;
      }
      return false;
    });
  }
}
