import 'package:tencent_rtc_sdk/trtc_cloud.dart';
import 'package:tencent_rtc_sdk/trtc_cloud_def.dart';

class BeautyService {
  final int _beautyStyle = 0;

  void setBeautyStyle(int beautyLevel, int whitenessLevel, int ruddinessLevel) async {
    TRTCCloud trtcCloud = await TRTCCloud.sharedInstance();
    trtcCloud.setBeautyStyle(TRTCBeautyStyleExt.fromValue(_beautyStyle), beautyLevel, whitenessLevel, ruddinessLevel);
  }
}
