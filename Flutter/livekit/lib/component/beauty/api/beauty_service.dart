import 'package:tencent_trtc_cloud/trtc_cloud.dart';
import 'package:tencent_trtc_cloud/tx_beauty_manager.dart';

import '../../../common/logger/index.dart';

class BeautyService {
  TXBeautyManager? beautyManager;

  Future<void> setBeautyLevel(int beautyLevel) async {
    LiveKitLogger.info("setBeautyLevel:[beautyLevel:$beautyLevel]");
    final manager = await _getBeautyManager();
    return manager?.setBeautyLevel(beautyLevel);
  }

  Future<void> setWhitenessLevel(int whitenessLevel) async {
    LiveKitLogger.info("setWhitenessLevel:[whitenessLevel:$whitenessLevel]");
    final manager = await _getBeautyManager();
    return manager?.setWhitenessLevel(whitenessLevel);
  }

  Future<void> setRuddyLevel(int ruddyLevel) async {
    LiveKitLogger.info("setRuddyLevel:[ruddyLevel:$ruddyLevel]");
    final manager = await _getBeautyManager();
    return manager?.setRuddyLevel(ruddyLevel);
  }
}

extension on BeautyService {
  Future<TXBeautyManager?> _getBeautyManager() async {
    if (beautyManager == null) {
      TRTCCloud? trtcCloud = await TRTCCloud.sharedInstance();
      beautyManager = trtcCloud?.getBeautyManager();
      beautyManager?.setBeautyStyle(0);
    }
    return beautyManager;
  }
}
