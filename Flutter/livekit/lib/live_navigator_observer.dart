import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';

import '../../common/index.dart';
import 'live_stream/widget/live_room/index.dart';

class TUILiveKitNavigatorObserver extends RouteObserver {
  static final TUILiveKitNavigatorObserver instance =
      TUILiveKitNavigatorObserver._internal();

  factory TUILiveKitNavigatorObserver() {
    return instance;
  }

  static const String routeLiveRoomAudience = "route_live_room_audience";
  static const String routeVoiceRoomAudience = "route_voice_room_audience";

  static bool isRepeatClick = false;

  TUILiveKitNavigatorObserver._internal() {
    LiveKitLogger.info('TUILiveKitNavigatorObserver Init');
    Boot.instance;
  }

  BuildContext getContext() {
    return navigator!.context;
  }

  void enterLiveRoomAudiencePage(TUILiveInfo liveInfo) {
    if (isRepeatClick) {
      return;
    }
    Navigator.push(
        getContext(),
        MaterialPageRoute(
          settings: const RouteSettings(name: routeLiveRoomAudience),
          builder: (context) {
            return TUILiveRoomAudienceWidget(roomId: liveInfo.roomInfo.roomId);
          },
        ));
    isRepeatClick = false;
  }

  void backToLiveRoomAudiencePage() {
    Navigator.popUntil(getContext(), (route) {
      if (route.settings.name == routeLiveRoomAudience) {
        return true;
      }
      return false;
    });
  }

  void enterVoiceRoomAudiencePage(TUILiveInfo liveInfo) {
    if (isRepeatClick) {
      return;
    }

    Navigator.push(
        getContext(),
        MaterialPageRoute(
          settings: const RouteSettings(name: routeVoiceRoomAudience),
          builder: (context) {
            final isOwner =
                liveInfo.roomInfo.ownerId == TUIRoomEngine.getSelfInfo().userId;
            return TUIVoiceRoomWidget(
                roomId: liveInfo.roomInfo.roomId,
                behavior:
                    isOwner ? RoomBehavior.autoCreate : RoomBehavior.join);
          },
        ));

    isRepeatClick = false;
  }

  void backToVoiceRoomAudiencePage() {
    Navigator.popUntil(getContext(), (route) {
      if (route.settings.name == routeVoiceRoomAudience) {
        return true;
      }
      return false;
    });
  }
}
