import 'package:flutter/cupertino.dart';

class LSCoGuestState {
  final ValueNotifier<Set<String>> lockAudioUserList = ValueNotifier({});
  final ValueNotifier<Set<String>> lockVideoUserList = ValueNotifier({});
  final ValueNotifier<CoGuestStatus> coGuestStatus =
      ValueNotifier(CoGuestStatus.none);
}

enum CoGuestStatus { none, applying, linking }
