import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/live_stream/live_define.dart';

import '../../../common/constants/constants.dart';

class EditInfo {
  ValueNotifier<String> roomName;
  ValueNotifier<String> coverUrl;
  ValueNotifier<LiveStreamPrivacyStatus> privacyMode;
  ValueNotifier<LiveTemplateMode> coGuestTemplateMode;
  ValueNotifier<LiveTemplateMode> coHostTemplateMode;

  EditInfo({
    String roomName = '',
    String coverUrl = Constants.defaultCoverUrl,
    LiveStreamPrivacyStatus privacyMode = LiveStreamPrivacyStatus.public,
    LiveTemplateMode coGuestTemplateMode = LiveTemplateMode.verticalDynamicGrid,
    LiveTemplateMode coHostTemplateMode = LiveTemplateMode.verticalDynamicGrid,
  })  : roomName = ValueNotifier(roomName),
        coverUrl = ValueNotifier(coverUrl),
        privacyMode = ValueNotifier(privacyMode),
        coGuestTemplateMode = ValueNotifier(coGuestTemplateMode),
        coHostTemplateMode = ValueNotifier(coHostTemplateMode);
}

enum Feature { beauty, audioEffect, flipCamera }

typedef DidClickBack = VoidCallback;
typedef DidClickStart = Function(EditInfo editInfo);
