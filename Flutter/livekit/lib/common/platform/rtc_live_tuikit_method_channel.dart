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
  static const _thermalEventChannel = EventChannel('tuilivekit_thermal_events');
  static const _networkEventChannel = EventChannel('tuilivekit_network_events');

  @override
  Stream<dynamic> get onRawChanged {
    return _thermalEventChannel.receiveBroadcastStream();
  }

  @override
  Stream<ThermalState> get onThermalStateChanged {
    return onRawChanged.map((value) {
      if (value is double) {
        return _mapAndroidTemp(value);
      } else if (value is int) {
        return _mapIOSThermal(value);
      } else {
        return ThermalState.nominal;
      }
    }).asBroadcastStream();
  }

  @override
  Stream<bool> get onNetworkConnectionStateChanged {
    final rawData = _networkEventChannel.receiveBroadcastStream();
    return rawData.map((value) {
      if (value is String) {
        return value == "connected" ? true : false;
      } else {
        return false;
      }
    }).asBroadcastStream();
  }

  @override
  Future<void> apiLog(LiveKitLoggerLevel level, String module, String file, int line, String logString) async {
    if (!kIsWeb && (Platform.isIOS || Platform.isAndroid)) {
      await methodChannel.invokeMethod(
          'apiLog', {'level': level.index, 'module': module, 'file': file, 'line': line, 'logString': logString});
    }
  }

  @override
  Future<void> startForegroundService(ForegroundServiceType type, String title, String description) async {
    if (Platform.isAndroid) {
      await methodChannel.invokeMethod(
          'startForegroundService', {'serviceType': type.index, 'title': title, 'description': description});
    }
  }

  @override
  Future<void> stopForegroundService(ForegroundServiceType type) async {
    if (Platform.isAndroid) {
      await methodChannel.invokeMethod('stopForegroundService', {'serviceType': type.index});
    }
  }

  @override
  Future<void> enableWakeLock(bool enable) async {
    await methodChannel.invokeMethod('enableWakeLock', {'enable': enable});
  }

  @override
  Future<void> openWifiSettings() async {
    await methodChannel.invokeMethod('openWifiSettings');
  }

  @override
  Future<void> openAppSettings() async {
    await methodChannel.invokeMethod('openAppSettings');
  }

  @override
  Future<bool> isNetworkConnected() async {
    final result = await methodChannel.invokeMethod<String>('getCurrentNetworkStatus');
    if (result == null) return false;
    return result == 'connected';
  }

  static ThermalState _mapAndroidTemp(double temp) {
    if (temp < 37) return ThermalState.nominal;
    if (temp < 40) return ThermalState.fair;
    if (temp < 46) return ThermalState.serious;
    return ThermalState.critical;
  }

  static ThermalState _mapIOSThermal(int state) {
    switch (state) {
      case 0:
        return ThermalState.nominal;
      case 1:
        return ThermalState.fair;
      case 2:
        return ThermalState.serious;
      case 3:
        return ThermalState.critical;
      default:
        return ThermalState.nominal;
    }
  }
}
