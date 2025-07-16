import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/live_stream/live_define.dart';

class EditInfo {
  ValueNotifier<String> roomName;
  ValueNotifier<String> coverUrl;
  ValueNotifier<LiveStreamPrivacyStatus> privacyMode;

  EditInfo({
    String roomName = '',
    String coverUrl = '',
    LiveStreamPrivacyStatus privacyMode = LiveStreamPrivacyStatus.public,
  })  : roomName = ValueNotifier(roomName),
        coverUrl = ValueNotifier(coverUrl),
        privacyMode = ValueNotifier(privacyMode);
}

enum Feature { beauty, audioEffect, flipCamera }

typedef DidClickBack = VoidCallback;
typedef DidClickStart = Function(EditInfo editInfo);
