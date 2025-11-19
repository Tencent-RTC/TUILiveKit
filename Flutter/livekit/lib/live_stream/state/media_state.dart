import 'package:flutter/cupertino.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

class LSMediaState {
  TUIAudioQuality audioQuality = TUIAudioQuality.audioProfileDefault;
  ValueNotifier<TUIVideoQuality> videoQuality = ValueNotifier(TUIVideoQuality.videoQuality_1080P);
  final ValueNotifier<bool> isAudioLocked = ValueNotifier(false);
  final ValueNotifier<bool> isVideoLocked = ValueNotifier(false);
  final ValueNotifier<TUIVideoQuality?> playbackQuality = ValueNotifier(null);
  final ValueNotifier<List<TUIVideoQuality>> playbackQualityList = ValueNotifier([]);
  final ValueNotifier<bool> isRemoteVideoStreamPaused = ValueNotifier(false);
  final ValueNotifier<int> currentPlayoutVolume = ValueNotifier(100);

  VideoEncParams videoEncParams = VideoEncParams.instance;
  VideoAdvancedSetting videoAdvancedSetting = VideoAdvancedSetting();
}

class VideoEncParams {
  VideoEncParams._();

  static VideoEncParams instance = VideoEncParams._();

  VideoEncType currentEncType = VideoEncType.big;

  TUIRoomVideoEncoderParams get currentEnc =>
      currentEncType == VideoEncType.small ? small : big;

  TUIRoomVideoEncoderParams big = TUIRoomVideoEncoderParams(
      videoResolution: TUIVideoQuality.videoQuality_1080P,
      resolutionMode: TUIResolutionMode.portrait,
      fps: 30,
      bitrate: 4000);
  TUIRoomVideoEncoderParams small = TUIRoomVideoEncoderParams(
      videoResolution: TUIVideoQuality.videoQuality_540P,
      resolutionMode: TUIResolutionMode.portrait,
      fps: 30,
      bitrate: 1800);

  void reset() {
    big = _getDefaultParam(VideoEncType.big);
    small = _getDefaultParam(VideoEncType.small);
  }

  TUIRoomVideoEncoderParams _getDefaultParam(VideoEncType encType) {
    return encType == VideoEncType.big
        ? TUIRoomVideoEncoderParams(
            videoResolution: TUIVideoQuality.videoQuality_1080P,
            resolutionMode: TUIResolutionMode.portrait,
            fps: 30,
            bitrate: 4000)
        : TUIRoomVideoEncoderParams(
            videoResolution: TUIVideoQuality.videoQuality_540P,
            resolutionMode: TUIResolutionMode.portrait,
            fps: 30,
            bitrate: 1800);
  }
}

enum VideoEncType { big, small }

class VideoAdvancedSetting {
  bool isVisible = false;
  bool isUltimateEnabled = false;
  bool isBFrameEnabled = false;
  bool isH265Enabled = false;
  HDRRenderType hdrRenderType = HDRRenderType.none;
}

enum HDRRenderType { none, displayLayer, metal }
