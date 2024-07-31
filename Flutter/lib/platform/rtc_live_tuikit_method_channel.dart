import 'dart:io';

import 'package:flutter/foundation.dart';
import 'package:flutter/services.dart';
import 'package:tencent_live_uikit/common/logger/index.dart';
import 'package:tencent_live_uikit/platform/rtc_live_tuikit_platform_interface.dart';

/// An implementation of [TUILiveKitPlatform] that uses method channels.
class MethodChannelTUILiveKit extends TUILiveKitPlatform {
  /// The method channel used to interact with the native platform.
  @visibleForTesting
  final methodChannel = const MethodChannel('tuilivekit');

  @override
  Future<void> apiLog(LiveKitLoggerLevel level, String logString) async {
    if (!kIsWeb && (Platform.isIOS || Platform.isAndroid)) {
      await methodChannel.invokeMethod('apiLog', {'level': level.index, 'logString': logString});
    }
  }
}
