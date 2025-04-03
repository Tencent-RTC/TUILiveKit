import 'package:tencent_live_uikit/common/platform/index.dart';

enum LiveKitLoggerLevel { info, warning, error }

class LiveKitLogger {
  static void info(String message) async {
    await TUILiveKitPlatform.instance.apiLog(LiveKitLoggerLevel.info, message);
  }

  static void warning(String message) async {
    await TUILiveKitPlatform.instance.apiLog(LiveKitLoggerLevel.warning, message);
  }

  static void error(String message) async {
    await TUILiveKitPlatform.instance.apiLog(LiveKitLoggerLevel.error, message);
  }
}
