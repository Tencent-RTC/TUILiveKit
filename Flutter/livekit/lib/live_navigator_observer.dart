import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/component/float_window/global_float_window_manager.dart';
import 'package:tencent_live_uikit/live_stream/features/index.dart';
import 'package:tencent_live_uikit/live_stream/features/live_room_audience_widget.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';

import '../../common/index.dart';
import 'component/float_window/global_float_window_state.dart';
import 'live_stream/features/live_room_audience_overlay.dart';

class TUILiveKitNavigatorObserver extends RouteObserver {
  static final TUILiveKitNavigatorObserver instance = TUILiveKitNavigatorObserver._internal();

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

  void enterLiveRoomAnchorPage(TUILiveInfo liveInfo) {
    if (isRepeatClick) {
      return;
    }
    Navigator.push(
        getContext(),
        MaterialPageRoute(
          settings: const RouteSettings(name: routeLiveRoomAudience),
          builder: (context) {
            if (GlobalFloatWindowManager.instance.isEnableFloatWindowFeature()) {
              return TUILiveRoomAnchorOverlay(roomId: liveInfo.roomId, needPrepare: false);
            } else {
              return TUILiveRoomAnchorWidget(roomId: liveInfo.roomId, needPrepare: false);
            }
          },
        ));
    isRepeatClick = false;
  }

  void enterLiveRoomAudiencePage(TUILiveInfo liveInfo) {
    if (isRepeatClick) {
      return;
    }
    GlobalFloatWindowManager floatWindowManager = GlobalFloatWindowManager.instance;
    GlobalFloatWindowState state = GlobalFloatWindowManager.instance.state;
    if (floatWindowManager.isFloating()) {
      if (floatWindowManager.state.roomId.value == liveInfo.roomId) {
        floatWindowManager.switchToFullScreenMode();
        return;
      } else if (state.ownerId.value == TUIRoomEngine.getSelfInfo().userId) {
        makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.livelist_exit_float_window_tip);
        return;
      } else {
        GlobalFloatWindowManager.instance.overlayManager.closeOverlay();
      }
    }
    Navigator.push(
        getContext(),
        MaterialPageRoute(
          settings: const RouteSettings(name: routeLiveRoomAudience),
          builder: (context) {
            if (GlobalFloatWindowManager.instance.isEnableFloatWindowFeature()) {
              return TUILiveRoomAudienceOverlay(roomId: liveInfo.roomId);
            } else {
              return TUILiveRoomAudienceWidget(roomId: liveInfo.roomId);
            }
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
    GlobalFloatWindowManager floatWindowManager = GlobalFloatWindowManager.instance;
    GlobalFloatWindowState state = GlobalFloatWindowManager.instance.state;
    if (floatWindowManager.isFloating()) {
      if (state.ownerId.value == TUIRoomEngine.getSelfInfo().userId) {
        makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.livelist_exit_float_window_tip);
        return;
      } else {
        GlobalFloatWindowManager.instance.overlayManager.closeOverlay();
      }
    }
    Navigator.push(
        getContext(),
        MaterialPageRoute(
          settings: const RouteSettings(name: routeVoiceRoomAudience),
          builder: (context) {
            final isOwner = liveInfo.ownerId == TUIRoomEngine.getSelfInfo().userId;
            return TUIVoiceRoomWidget(
                roomId: liveInfo.roomId, behavior: isOwner ? RoomBehavior.autoCreate : RoomBehavior.join);
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
