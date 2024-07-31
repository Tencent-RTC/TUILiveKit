import 'package:flutter_test/flutter_test.dart';
import 'package:plugin_platform_interface/plugin_platform_interface.dart';
import 'package:tencent_live_uikit/common/logger/logger.dart';
import 'package:tencent_live_uikit/platform/rtc_live_tuikit_method_channel.dart';
import 'package:tencent_live_uikit/platform/rtc_live_tuikit_platform_interface.dart';

class MockTuilivekitPlatform
    with MockPlatformInterfaceMixin
    implements TUILiveKitPlatform {
  @override
  Future<void> apiLog(LiveKitLoggerLevel level, String logString) {
    // TODO: implement apiLog
    throw UnimplementedError();
  }

}

void main() {
  final TUILiveKitPlatform initialPlatform = TUILiveKitPlatform.instance;

  test('$MethodChannelTUILiveKit is the default instance', () {
    expect(initialPlatform, isInstanceOf<MethodChannelTUILiveKit>());
  });
}
