import 'package:flutter/material.dart';

class MediaState {
  ValueNotifier<bool> isMicrophoneOpened = ValueNotifier(false);
  ValueNotifier<bool> isMicrophoneMuted = ValueNotifier(true);
}