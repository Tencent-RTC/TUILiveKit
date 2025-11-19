import 'package:rtc_room_engine/rtc_room_engine.dart';

import '../../api/live_stream_service.dart';
import '../../state/co_guest_state.dart';
import '../../state/media_state.dart';
import '../live_stream_manager.dart';

class MediaManager {
  LSMediaState mediaState = LSMediaState();

  late final Context context;
  late final LiveStreamService service;

  void init(Context context) {
    this.context = context;
    service = context.service;
    _enableMultiPlaybackQuality(true);
    _enableSwitchPlaybackQuality(true);
  }

  void dispose() {
    _enableMultiPlaybackQuality(false);
    _enableSwitchPlaybackQuality(false);
  }

  void prepareLiveInfoBeforeEnterRoom(TUILiveInfo liveInfo) {
    _enableMultiPlaybackQuality(true);
  }

  void onJoinLive(TUILiveInfo liveInfo) async {
    final result = await getMultiPlaybackQuality(liveInfo.roomId);
    if (result.code != TUIError.success || result.data == null) {
      return;
    }
    final List<TUIVideoQuality> playbackQualityList = result.data as List<TUIVideoQuality>;
    mediaState.playbackQualityList.value = playbackQualityList;
  }

  void onLeaveLive() {
    mediaState = LSMediaState();
  }

  void onStopLive() {
    mediaState = LSMediaState();
    _enableMultiPlaybackQuality(false);
  }

  void setLocalVideoView(int viewId) {
    service.setLocalVideoView(viewId);
  }

  void onCameraOpened() {
    service.enableGravitySensor(true);
  }

  void updateVideoQuality(TUIVideoQuality quality) {
    service.updateVideoQuality(quality);
    mediaState.videoQuality.value = quality;
  }

  Future<TUIValueCallBack<List<TUIVideoQuality>>> getMultiPlaybackQuality(String roomId) {
    return service.queryPlaybackQualityList(roomId);
  }

  void switchPlaybackQuality(TUIVideoQuality videoQuality) {
    service.switchPlaybackQuality(videoQuality);
    mediaState.playbackQuality.value = videoQuality;
  }

  void setAudioPlayoutVolume(int volume) {
    service.setAudioPlayoutVolume(volume);
    mediaState.currentPlayoutVolume.value = volume;
  }

  void pauseByAudience() {
    service.pauseByAudience();
    mediaState.isRemoteVideoStreamPaused.value = true;
  }

  void resumeByAudience() {
    service.resumeByAudience();
    mediaState.isRemoteVideoStreamPaused.value = false;
  }

  void onSelfMediaDeviceStateChanged(TUISeatInfo seatInfo) {
    mediaState.isAudioLocked.value = seatInfo.isAudioLocked ?? false;
    mediaState.isVideoLocked.value = seatInfo.isVideoLocked ?? false;
  }

  void onSelfLeaveSeat() {
    mediaState.isAudioLocked.value = false;
    mediaState.isVideoLocked.value = false;
  }

  void onUserVideoSizeChanged(String roomId, String userId, TUIVideoStreamType streamType, int width, int height) {
    final playbackQuality = _getVideoQualityByVideoSize(width, height);
    if (playbackQuality == mediaState.playbackQuality.value) {
      return;
    }
    if (mediaState.playbackQualityList.value.length <= 1 ||
        !mediaState.playbackQualityList.value.contains(playbackQuality)) {
      return;
    }
    if (context.coGuestManager.target?.coGuestState.coGuestStatus.value != CoGuestStatus.none) {
      return;
    }
    mediaState.playbackQuality.value = playbackQuality;
  }
}

extension on MediaManager {
  void _enableMultiPlaybackQuality(bool enable) {
    service.enableMultiPlaybackQuality(enable);
  }

  void _enableSwitchPlaybackQuality(bool enable) {
    service.enableSwitchPlaybackQuality(enable);
  }

  TUIVideoQuality _getVideoQualityByVideoSize(int width, int height) {
    if (width * height <= 360 * 640) {
      return TUIVideoQuality.videoQuality_360P;
    }
    if (width * height <= 540 * 960) {
      return TUIVideoQuality.videoQuality_540P;
    }
    if (width * height <= 720 * 1280) {
      return TUIVideoQuality.videoQuality_720P;
    }
    return TUIVideoQuality.videoQuality_1080P;
  }
}
