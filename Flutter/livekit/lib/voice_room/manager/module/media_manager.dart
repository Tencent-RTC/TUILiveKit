import '../../index.dart';

class MediaManager {
  final MediaState state = MediaState();
  late final Context context;
  late final VoiceRoomService service;

  void init(Context context) {
    this.context = context;
    service = context.service;
  }

  void onMicrophoneOpened() {
    state.isMicrophoneOpened.value = true;
  }

  void onMicrophoneClosed() {
    state.isMicrophoneOpened.value = false;
  }

  void onMicrophoneMute() {
    state.isMicrophoneMuted.value = true;
  }

  void onMicrophoneUnmute() {
    state.isMicrophoneMuted.value = false;
  }
}
