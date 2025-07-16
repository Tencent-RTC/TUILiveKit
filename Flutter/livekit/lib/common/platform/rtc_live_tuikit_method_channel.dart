import 'dart:io';

import 'package:flutter/foundation.dart';
import 'package:flutter/services.dart';
import 'package:tencent_live_uikit/common/logger/index.dart';

import 'index.dart';

/// An implementation of [TUILiveKitPlatform] that uses method channels.
class MethodChannelTUILiveKit extends TUILiveKitPlatform {
  /// The method channel used to interact with the native platform.
  @visibleForTesting
  final methodChannel = const MethodChannel('tuilivekit');

  @override
  Future<void> apiLog(LiveKitLoggerLevel level, String module, String file,
      int line, String logString) async {
    if (!kIsWeb && (Platform.isIOS || Platform.isAndroid)) {
      await methodChannel.invokeMethod('apiLog', {
        'level': level.index,
        'module': module,
        'file': file,
        'line': line,
        'logString': logString
      });
    }
  }

  @override
  Future<void> startForegroundService(
      ForegroundServiceType type, String title, String description) async {
    if (Platform.isAndroid) {
      await methodChannel.invokeMethod('startForegroundService', {
        'serviceType': type.index,
        'title': title,
        'description': description
      });
    }
  }

  @override
  Future<void> stopForegroundService(ForegroundServiceType type) async {
    if (Platform.isAndroid) {
      await methodChannel
          .invokeMethod('stopForegroundService', {'serviceType': type.index});
    }
  }
}
