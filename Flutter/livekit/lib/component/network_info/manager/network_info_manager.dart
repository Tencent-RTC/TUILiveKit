import 'dart:async';

import 'package:collection/collection.dart';

import 'package:permission_handler/permission_handler.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/component/network_info/manager/network_info_service.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';
import 'package:tencent_rtc_sdk/trtc_cloud_def.dart';
import 'package:tencent_rtc_sdk/trtc_cloud_listener.dart';

import '../state/network_info_state.dart';

class NetworkInfoManager extends TUIRoomObserver {
  final NetWorkInfoState state = NetWorkInfoState();
  final NetworkInfoService _service = NetworkInfoService();
  Timer? _poorNetworkTimer;
  bool _hasAudioPermission = false;
  bool _hasVideoPermission = false;
  bool _isNetworkAvailable = false;
  late TRTCCloudListener _trtcObserver;
  bool _isDisposed = false;
  int _countdown = 0;
  StreamSubscription<bool>? _networkStatusSubscription;

  NetworkInfoManager() {
    super.onUserNetworkQualityChanged = (networkMap) async {
      if (!_isNetworkAvailable) {
        return;
      }

      final userId = _service.getSelfUserId();
      if (!networkMap.containsKey(userId)) {
        return;
      }
      final matchedInfo = networkMap[userId];
      state.rtt.value = matchedInfo!.delay;
      state.downLoss.value = matchedInfo!.downLoss;
      state.upLoss.value = matchedInfo!.upLoss;
      if (_isNetworkAvailable) {
        state.networkQuality.value = matchedInfo!.quality;
      }
      _handleNetworkQualityChanged(matchedInfo!.quality);
    };

    super.onUserAudioStateChanged = (userId, hasAudio, reason) {
      if (userId != _service.getSelfUserId()) {
        return;
      }
      state.audioState.value = hasAudio ? AudioState.normal : AudioState.close;
    };

    super.onUserVideoStateChanged = (userId, streamType, hasVideo, reason) {
      if (userId != _service.getSelfUserId()) {
        return;
      }
      state.videoState.value = hasVideo ? VideoState.normal : VideoState.close;
    };

    _trtcObserver = _getTRTCObserver();
    _service.addTRTCObserver(_trtcObserver);
    _service.addRoomEngineObserver(this);
    _addThermalObserver();
    _initManager();
  }

  void dispose() {
    _service.removeTRTCObserver(_trtcObserver);
    _service.removeRoomEngineObserver(this);
    _poorNetworkTimer?.cancel();
    _poorNetworkTimer = null;
    _stopNetworkMonitoring();
    _isDisposed = true;
  }
}

extension NetowrkInfoManagerWithCallback on NetworkInfoManager {
  void handleAudioSliderChanged(int volume) {
    _service.setAudioCaptureVolume(volume);
    state.volume.value = volume;
  }

  void onNetworkInfoStatusToastWidgetClosed() {
    _poorNetworkTimer?.cancel();
    _poorNetworkTimer = null;
    state.showToast.value = false;
  }

  void onAudioQualityChanged(TUIAudioQuality quality) {
    _service.updateAudioQuality(quality);
    state.audioQuality.value = quality;
  }
}

extension on NetworkInfoManager {
  TRTCCloudListener _getTRTCObserver() {
    return TRTCCloudListener(onStatistics: (statistics) {
      final bigStreamStatus =
          statistics.localStatisticsArray?.firstWhereOrNull((s) => s.streamType == TRTCVideoStreamType.big);
      if (bigStreamStatus == null) {
        return;
      }

      final videoWidth = bigStreamStatus.width;
      state.videoResolution.value = videoWidth;
      if (!_hasVideoPermission) {
        return;
      }

      if (state.videoState.value != VideoState.close) {
        final isLowFrameRate = bigStreamStatus.frameRate < 15;
        bool isLowBitrate = false;
        switch (videoWidth) {
          case 240:
            isLowBitrate = bigStreamStatus.videoBitrate < 100;
            break;
          case 360:
            isLowBitrate = bigStreamStatus.videoBitrate < 200;
            break;
          case 480:
            isLowBitrate = bigStreamStatus.videoBitrate < 350;
          case 540:
            isLowBitrate = bigStreamStatus.videoBitrate < (bigStreamStatus.height == 960 ? 800 : 500);
            break;
          case 1080:
            isLowBitrate = bigStreamStatus.videoBitrate < 1500;
            break;
          default:
            isLowBitrate = false;
            break;
        }
        state.videoState.value = (isLowFrameRate || _isPoorNetworkQuality(state.networkQuality.value) || isLowBitrate)
            ? VideoState.exception
            : VideoState.normal;
      }

      if (!_hasAudioPermission) {
        return;
      }

      if (state.volume.value == 0) {
        state.audioState.value = AudioState.mute;
        return;
      }
      const audioCaptureStateNormal = 0;
      if (bigStreamStatus.audioCaptureState != audioCaptureStateNormal) {
        state.audioState.value = AudioState.exception;
        return;
      }

      state.audioState.value = AudioState.normal;
    });
  }

  void _addThermalObserver() {
    TUILiveKitPlatform.instance.onThermalStateChanged.listen((status) {
      state.deviceTemperature.value = status.value;
    });
  }

  void _initManager() async {
    _startNetworkMonitoring();
    _checkMediaPermission();
    state.volume.value = await _service.getVolume();
    _isNetworkAvailable = await TUILiveKitPlatform.instance.isNetworkConnected();
  }

  void _startNetworkMonitoring() {
    _networkStatusSubscription?.cancel();
    _networkStatusSubscription = null;

    _networkStatusSubscription = TUILiveKitPlatform.instance.onNetworkConnectionStateChanged.listen((connected) {
      _isNetworkAvailable = connected;
      if (!_isNetworkAvailable) {
        state.networkQuality.value = TUINetworkQuality.qualityDown;
        state.videoState.value = VideoState.exception;
      }
    });
  }

  void _stopNetworkMonitoring() {
    _networkStatusSubscription?.cancel();
    _networkStatusSubscription = null;
  }

  void _checkMediaPermission() async {
    _hasAudioPermission = await Permission.microphone.isGranted;
    _hasVideoPermission = await Permission.camera.isGranted;

    if (!_hasAudioPermission) {
      state.audioState.value = AudioState.close;
    }

    if (!_hasVideoPermission) {
      state.videoState.value = VideoState.close;
    }
  }

  bool _isPoorNetworkQuality(TUINetworkQuality quality) {
    return quality == TUINetworkQuality.qualityVeryBad ||
        quality == TUINetworkQuality.qualityBad ||
        quality == TUINetworkQuality.qualityDown;
  }

  void _handleNetworkQualityChanged(TUINetworkQuality quality) {
    if (state.showToast.value) {
      return;
    }
    _isPoorNetworkQuality(quality) ? _startPoorNetworkCountdown() : _stopPoorNetworkCountdown();
  }

  void _startPoorNetworkCountdown() {
    if (_isDisposed) {
      return;
    }
    if (_poorNetworkTimer != null) {
      return;
    }

    const countdownTimeInSeconds = 30;
    _countdown = countdownTimeInSeconds;
    _poorNetworkTimer?.cancel();
    _poorNetworkTimer = Timer.periodic(const Duration(seconds: 1), (_) {
      if (_isDisposed) {
        _poorNetworkTimer?.cancel();
        _poorNetworkTimer = null;
        return;
      }

      _countdown -= 1;
      if (_countdown <= 0) {
        state.showToast.value = true;
        _poorNetworkTimer?.cancel();
        _poorNetworkTimer = null;

        Future.delayed(const Duration(seconds: 5), () {
          if (_isDisposed) {
            return;
          }
          state.showToast.value = false;
        });
      }
    });
  }

  void _stopPoorNetworkCountdown() {
    _poorNetworkTimer?.cancel();
    _poorNetworkTimer = null;
    state.showToast.value = false;
  }
}
