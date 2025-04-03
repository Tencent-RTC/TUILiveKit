import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

class MediaState {
  final ValueNotifier<bool> hasMicrophonePermission = ValueNotifier<bool>(false);
  final ValueNotifier<bool> isMicrophoneOpened = ValueNotifier<bool>(false);
  final ValueNotifier<bool> isMicrophoneMuted = ValueNotifier<bool>(true);
  final ValueNotifier<TUIAudioQuality> audioQuality =
      ValueNotifier<TUIAudioQuality>(TUIAudioQuality.audioProfileDefault);
  final ValueNotifier<bool> hasCameraPermission = ValueNotifier<bool>(false);
  final ValueNotifier<bool> isCameraOpened = ValueNotifier<bool>(false);
  final ValueNotifier<TUIVideoQuality> videoQuality =
      ValueNotifier<TUIVideoQuality>(TUIVideoQuality.videoQuality_1080P);
  final ValueNotifier<bool> isMirror = ValueNotifier<bool>(true);
  final ValueNotifier<bool> isFrontCamera = ValueNotifier<bool>(true);

  void reset() {
    hasMicrophonePermission.value = false;
    isMicrophoneOpened.value = false;
    isMicrophoneMuted.value = true;
    audioQuality.value = TUIAudioQuality.audioProfileDefault;
    hasCameraPermission.value = false;
    isCameraOpened.value = false;
    videoQuality.value = TUIVideoQuality.videoQuality_1080P;
    isMirror.value = true;
    isFrontCamera.value = true;
  }
}
