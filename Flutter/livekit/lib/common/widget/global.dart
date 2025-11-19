import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/live_navigator_observer.dart';

class Global {
  static GlobalKey<NavigatorState> navigatorKey = GlobalKey();
  static GlobalKey<NavigatorState> secondaryNavigatorKey = GlobalKey();

  static BuildContext appContext() {
    return TUILiveKitNavigatorObserver.instance.getContext();
  }
}
