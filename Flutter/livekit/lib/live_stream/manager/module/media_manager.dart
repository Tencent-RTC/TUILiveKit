import 'package:rtc_room_engine/api/room/tui_room_define.dart';

import '../../api/live_stream_service.dart';
import '../../state/media_state.dart';
import '../live_stream_manager.dart';

class MediaManager {
  LSMediaState mediaState = LSMediaState();

  late final Context context;
  late final LiveStreamService service;

  void init(Context context) {
    this.context = context;
    service = context.service;
  }

  void dispose() {}

  void setLocalVideoView(int viewId) {
    service.setLocalVideoView(viewId);
  }

  void onCameraOpened() {
    service.enableGravitySensor(true);
    service.setVideoResolutionMode(TUIResolutionMode.portrait);
  }

  void updateVideoQuality(TUIVideoQuality quality) {
    service.updateVideoQuality(quality);
    mediaState.videoQuality = quality;
  }

  void onLeaveLive() {
    mediaState = LSMediaState();
  }

  void onSelfMediaDeviceStateChanged(TUISeatInfo seatInfo) {
    mediaState.isAudioLocked.value = seatInfo.isAudioLocked ?? false;
    mediaState.isVideoLocked.value = seatInfo.isVideoLocked ?? false;
  }

  void onSelfLeaveSeat() {
    mediaState.isAudioLocked.value = false;
    mediaState.isVideoLocked.value = false;
  }
}
