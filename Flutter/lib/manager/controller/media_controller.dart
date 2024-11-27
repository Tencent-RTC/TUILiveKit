import 'dart:ui';

import 'package:permission_handler/permission_handler.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/manager/controller/controller.dart';
import 'package:tencent_live_uikit/state/index.dart';

class MediaController extends Controller {
  static const String tag = 'MediaController';
  late VoidCallback linkStateObserver = () => _onLinkStateChanged();

  MediaController(super.state, super.service) {
    viewState.linkStatus.addListener(linkStateObserver);
  }

  @override
  void destroy() {
    viewState.linkStatus.removeListener(linkStateObserver);
  }

  void operateMicrophone() {
    final isMicrophoneOpened = mediaState.isMicrophoneOpened.value;
    if (!isMicrophoneOpened) {
      unMuteLocalAudio();
      openLocalMicrophone();
      return;
    }
    final isMuted = mediaState.isMicrophoneMuted.value;
    if (isMuted) {
      unMuteLocalAudio();
    } else {
      muteLocalAudio();
    }
  }

  Future<TUIActionCallback> openLocalMicrophone() async {
    var microphone = await Permission.microphone.request();

    if (!microphone.isGranted) {
      LiveKitLogger.error('requestMicrophonePermissions:[onDenied]');
      return TUIActionCallback(code: TUIError.errPermissionDenied, message: "camera permission denied");
    }
    unMuteLocalAudio();
    var result = await liveService.openLocalMicrophone(TUIAudioQuality.audioProfileDefault);
    if (result.code == TUIError.success) {
      mediaState.isMicrophoneOpened.value = true;
    }
    return result;
  }

  Future<TUIActionCallback> unMuteLocalAudio() async {
    var result = await liveService.unMuteLocalAudio();
    if (result.code == TUIError.success) {
      mediaState.isMicrophoneMuted.value = false;
    }
    return result;
  }

  Future<TUIActionCallback> muteLocalAudio() async {
    var result = await liveService.muteLocalAudio();
    if (result.code == TUIError.success) {
      mediaState.isMicrophoneMuted.value = true;
    }
    return result;
  }

  void setLocalVideoView(int viewId) {
    liveService.setLocalVideoView(viewId);
  }

  void setRemoteVideoView(String userId, TUIVideoStreamType type, int viewId) {
    liveService.setRemoteVideoView(userId, type, viewId);
  }

  void startPlayRemoteVideo(String userId, TUIVideoStreamType type, TUIPlayCallback? playCallback) {
    liveService.startPlayRemoteVideo(userId, type, playCallback);
  }

  void stopPlayRemoteVideo(String userId, TUIVideoStreamType type) {
    liveService.stopPlayRemoteVideo(userId, type);
  }

  Future<TUIActionCallback> openLocalCamera() async {
    var cameraPermission = await Permission.camera.request();

    if (!cameraPermission.isGranted) {
      LiveKitLogger.error('requestMicrophonePermissions:[onDenied]');
      return TUIActionCallback(code: TUIError.errPermissionDenied, message: "camera permission denied");
    }

    var result = await liveService.openLocalCamera(mediaState.isFrontCamera.value, mediaState.videoQuality.value);
    if (result.code == TUIError.success) {
      _initLivingConfig();
      mediaState.isCameraOpened.value = true;
    }
    return result;
  }

  void closeLocalCamera() {
    liveService.closeLocalCamera();
    mediaState.isCameraOpened.value = false;
  }

  void switchCamera() async {
    bool isFrontCamera = mediaState.isFrontCamera.value;
    await liveService.switchCamera(!isFrontCamera);
    mediaState.isFrontCamera.value = !isFrontCamera;
  }

  void setCameraMirror() {
    bool isMirror = mediaState.isMirror.value;
    liveService.setCameraMirror(!isMirror);
    mediaState.isMirror.value = !isMirror;
  }

  void updateVideoQuality(TUIVideoQuality quality) {
    liveService.updateVideoQuality(quality);
    mediaState.videoQuality.value = quality;
  }

  void updateAudioQuality(TUIAudioQuality quality) {
    liveService.updateAudioQuality(quality);
    mediaState.audioQuality.value = quality;
  }

  void setBeautyLevel(int level) async {
    await liveService.setBeautyLevel(level);
    beautyState.smoothLevel.value = level;
  }

  void setWhitenessLevel(int level) async {
    await liveService.setWhitenessLevel(level);
    beautyState.whitenessLevel.value = level;
  }

  void setRuddyLevel(int level) async {
    await liveService.setRuddyLevel(level);
    beautyState.ruddyLevel.value = level;
  }

  void closeBeauty() {
    beautyState.smoothLevel.value = 0;
    liveService.setBeautyLevel(0);
    beautyState.whitenessLevel.value = 0;
    liveService.setWhitenessLevel(0);
    beautyState.ruddyLevel.value = 0;
    liveService.setRuddyLevel(0);
  }

  void _initLivingConfig() {
    liveService.enableGravitySensor(true);
    liveService.setVideoResolutionMode(TUIVideoStreamType.cameraStream, TUIResolutionMode.portrait);
    liveService.setBeautyStyle(0);
    updateVideoQuality(mediaState.videoQuality.value);
    setBeautyLevel(beautyState.smoothLevel.value);
    setWhitenessLevel(beautyState.whitenessLevel.value);
    setRuddyLevel(beautyState.ruddyLevel.value);
    updateAudioQuality(mediaState.audioQuality.value);
  }

  void _onLinkStateChanged() async {
    LiveKitLogger.info("$tag onUserStateChanged status:${viewState.linkStatus.value}.");
    if (viewState.linkStatus.value == LinkStatus.linking) {
      if (viewState.autoOpenCameraOnSeated.value) {
        LiveKitLogger.info("$tag requestPermissions:[Camera, Microphone]}");
        final openMicrophoneResult = await openLocalMicrophone();
        if (TUIError.success != openMicrophoneResult.code) {
          LiveKitLogger.error("$tag openLocalMicrophone:[message: ${openMicrophoneResult.message}]");
        }
        final openCameraResult = await openLocalCamera();
        if (TUIError.success == openCameraResult.code) {
          LiveKitLogger.error("$tag openLocalCamera:[message: ${openMicrophoneResult.message}]");
        }
      } else {
        await openLocalMicrophone();
      }
    }
  }
}
