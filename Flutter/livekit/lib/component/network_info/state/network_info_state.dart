import 'package:flutter/cupertino.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

class NetWorkInfoState {
  ValueNotifier<VideoState> videoState = ValueNotifier(VideoState.normal);
  ValueNotifier<int> videoResolution = ValueNotifier(720);

  ValueNotifier<AudioState> audioState = ValueNotifier(AudioState.normal);
  ValueNotifier<TUIAudioQuality> audioQuality = ValueNotifier(TUIAudioQuality.audioProfileDefault);
  ValueNotifier<int> volume = ValueNotifier(50);

  ValueNotifier<int> rtt = ValueNotifier(0);
  ValueNotifier<int> upLoss = ValueNotifier(9);
  ValueNotifier<int> downLoss = ValueNotifier(0);
  ValueNotifier<int> uploadSpeed = ValueNotifier(0);
  ValueNotifier<TUINetworkQuality> networkQuality = ValueNotifier(TUINetworkQuality.qualityExcellent);

  ValueNotifier<int> deviceTemperature = ValueNotifier(0);
  ValueNotifier<bool> showToast = ValueNotifier(false);
}

enum AudioState { mute, close, normal, exception }

enum VideoState { close, normal, exception }
