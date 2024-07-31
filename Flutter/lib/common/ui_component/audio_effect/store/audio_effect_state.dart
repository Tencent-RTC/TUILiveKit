import 'package:flutter/material.dart';

class AudioEffectState {
  ValueNotifier<int> voiceVolume = ValueNotifier(100);
  ValueNotifier<int> musicVolume = ValueNotifier(100);
  ValueNotifier<int> earMonitorVolume = ValueNotifier(100);
  ValueNotifier<bool> enableVoiceEarMonitor = ValueNotifier(false);
  ValueNotifier<int> changerType = ValueNotifier(0);
  ValueNotifier<int> reverbType = ValueNotifier(0);
}