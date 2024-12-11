import 'package:plugin_platform_interface/plugin_platform_interface.dart';
import 'package:tencent_live_uikit/platform/rtc_live_tuikit_method_channel.dart';

import 'package:tencent_live_uikit/common/logger/index.dart';

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

  Future<void> apiLog(LiveKitLoggerLevel level, String logString) async {
    await instance.apiLog(level, logString);
  }
}
