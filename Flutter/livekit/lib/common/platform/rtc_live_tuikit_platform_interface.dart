import 'package:plugin_platform_interface/plugin_platform_interface.dart';
import 'package:tencent_live_uikit/common/logger/index.dart';

import 'rtc_live_tuikit_method_channel.dart';

abstract class TUILiveKitPlatform extends PlatformInterface {
  /// Constructs a TuilivekitPlatform.
  TUILiveKitPlatform() : super(token: _token);

  static final Object _token = Object();

  static TUILiveKitPlatform _instance = MethodChannelTUILiveKit();

  /// The default instance of [TUILiveKitPlatform] to use.
  ///
  /// Defaults to [MethodChannelTUILiveKit].
  static TUILiveKitPlatform get instance => _instance;

  /// Platform-specific implementations should set this with their own
  /// platform-specific class that extends [TUILiveKitPlatform] when
  /// they register themselves.
  static set instance(TUILiveKitPlatform instance) {
    PlatformInterface.verifyToken(instance, _token);
    _instance = instance;
  }

  Future<void> apiLog(LiveKitLoggerLevel level, String module, String file,
      int line, String logString) async {
    await instance.apiLog(level, module, file, line, logString);
  }

  Future<void> startForegroundService(
      ForegroundServiceType type, String title, String description) async {
    await instance.startForegroundService(type, title, description);
  }

  Future<void> stopForegroundService(ForegroundServiceType type) async {
    await instance.stopForegroundService(type);
  }
}

enum ForegroundServiceType { video, audio, media }
