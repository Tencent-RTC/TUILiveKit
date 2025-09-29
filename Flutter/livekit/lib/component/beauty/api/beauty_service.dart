import 'package:tencent_rtc_sdk/trtc_cloud.dart';
import 'package:tencent_rtc_sdk/trtc_cloud_def.dart';

import '../../../common/logger/index.dart';

class BeautyService {
  final int _beautyStyle = 0;
  int _beautyLevel = 0;
  int _whitenessLevel = 0;
  int _ruddinessLevel = 0;

  Future<void> setBeautyLevel(int beautyLevel) async {
    LiveKitLogger.info("setBeautyLevel:[beautyLevel:$beautyLevel]");
    _beautyLevel = beautyLevel;
    _setBeautyStyle();
  }

  Future<void> setWhitenessLevel(int whitenessLevel) async {
    LiveKitLogger.info("setWhitenessLevel:[whitenessLevel:$whitenessLevel]");
    _whitenessLevel = whitenessLevel;
    _setBeautyStyle();
  }

  Future<void> setRuddyLevel(int ruddyLevel) async {
    LiveKitLogger.info("setRuddyLevel:[ruddyLevel:$ruddyLevel]");
    _ruddinessLevel = ruddyLevel;
    _setBeautyStyle();
  }
}

extension on BeautyService {
  void _setBeautyStyle() async {
    TRTCCloud trtcCloud = await TRTCCloud.sharedInstance();
    trtcCloud.setBeautyStyle(
        TRTCBeautyStyleExt.fromValue(_beautyStyle), _beautyLevel, _whitenessLevel, _ruddinessLevel);
  }
}
