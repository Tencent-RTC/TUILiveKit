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

  Stream<dynamic> get onRawChanged;

  Stream<ThermalState> get onThermalStateChanged;

  static Stream<ThermalState> get thermalStateChanged => instance.onThermalStateChanged;

  Stream<bool> get onNetworkConnectionStateChanged;

  static Stream<bool> get networkConnectionStateChanged => instance.onNetworkConnectionStateChanged;

  Future<void> apiLog(LiveKitLoggerLevel level, String module, String file, int line, String logString) async {
    await instance.apiLog(level, module, file, line, logString);
  }

  Future<void> startForegroundService(ForegroundServiceType type, String title, String description) async {
    await instance.startForegroundService(type, title, description);
  }

  Future<void> stopForegroundService(ForegroundServiceType type) async {
    await instance.stopForegroundService(type);
  }

  Future<void> enableWakeLock(bool enable) async {
    await instance.enableWakeLock(enable);
  }

  Future<void> openWifiSettings() async {
    await instance.openWifiSettings();
  }

  Future<void> openAppSettings() async {
    await instance.openAppSettings();
  }

  Future<bool> isNetworkConnected() {
    return _instance.isNetworkConnected();
  }
}

enum ForegroundServiceType { video, audio, media }

enum ThermalState {
  nominal(0),
  fair(1),
  serious(2),
  critical(3);

  final int value;

  const ThermalState(this.value);

  static ThermalState fromInt(int value) {
    return ThermalState.values.firstWhere(
      (e) => e.value == value,
      orElse: () => ThermalState.nominal,
    );
  }
}
