import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

class Global {
  static GlobalKey<NavigatorState> navigatorKey = GlobalKey();

  static BuildContext appContext() {
    return TUILiveKitNavigatorObserver.instance.getContext();
  }
}
