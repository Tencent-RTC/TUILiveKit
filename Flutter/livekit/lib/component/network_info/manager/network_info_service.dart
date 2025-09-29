import 'package:tencent_live_uikit/tencent_live_uikit.dart';
import 'package:tencent_rtc_sdk/trtc_cloud.dart';
import 'package:tencent_rtc_sdk/trtc_cloud_def.dart';
import 'package:tencent_rtc_sdk/trtc_cloud_listener.dart';

class NetworkInfoService {
  final TUIRoomEngine roomEngine = TUIRoomEngine.sharedInstance();
  TRTCCloud? trtcCloud;

  void addTRTCObserver(TRTCCloudListener observer) async {
    final trtcCloud = await _getTRTCCloud();
    trtcCloud.registerListener(observer);
  }

  void removeTRTCObserver(TRTCCloudListener observer) async {
    final trtcCloud = await _getTRTCCloud();
    trtcCloud.unRegisterListener(observer);
  }

  void addRoomEngineObserver(TUIRoomObserver observer) {
    roomEngine.addObserver(observer);
  }

  void removeRoomEngineObserver(TUIRoomObserver observer) {
    roomEngine.removeObserver(observer);
  }

  void setAudioCaptureVolume(int volume) async {
    final trtcCloud = await _getTRTCCloud();
    trtcCloud.setAudioCaptureVolume(volume);
  }

  String getSelfUserId() {
    return TUIRoomEngine.getSelfInfo().userId;
  }

  Future<int> getVolume() async {
    final trtcCloud = await _getTRTCCloud();
    return trtcCloud.getAudioCaptureVolume();
  }

  void setVideoResolution(TRTCVideoResolution resolution) async {
    final params = TRTCVideoEncParam();
    params.videoResolution = resolution;
    final trtcCloud = await _getTRTCCloud();
    trtcCloud.setVideoEncoderParam(params);
  }

  void updateAudioQuality(TUIAudioQuality quality) {
    roomEngine.updateAudioQuality(quality);
  }
}

extension on NetworkInfoService {
  Future<TRTCCloud> _getTRTCCloud() async {
    trtcCloud ??= await TRTCCloud.sharedInstance();
    return trtcCloud!;
  }
}
