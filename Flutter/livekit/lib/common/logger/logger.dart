import 'package:tencent_live_uikit/common/platform/index.dart';
import 'package:stack_trace/stack_trace.dart';

import '../constants/constants.dart';

enum LiveKitLoggerLevel { info, warning, error }

class LiveKitLogger {
  static const String moduleName = Constants.liveKitLog;

  static void info(String message) async {
    await _log(LiveKitLoggerLevel.info, message);
  }

  static void warning(String message) async {
    await _log(LiveKitLoggerLevel.warning, message);
  }

  static void error(String message) async {
    await _log(LiveKitLoggerLevel.error, message);
  }

  static Future<void> _log(LiveKitLoggerLevel level, String message) async {
    var chain = Chain.current();
    chain =
        chain.foldFrames((frame) => frame.isCore || frame.package == "flutter");
    final frames = chain.toTrace().frames;
    final index =
        frames.indexWhere((frame) => frame.member == 'LiveKitLogger._log');
    if (index == -1 || index + 2 >= frames.length) return;

    final frame = frames[index + 2];
    final file = frame.uri.toString().split('/').last;
    final line = frame.line ?? 0;

    TUILiveKitPlatform.instance.apiLog(level, moduleName, file, line, message);
  }
}
