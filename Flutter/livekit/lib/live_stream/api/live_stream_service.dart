import 'dart:convert';

import 'package:flutter/cupertino.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_rtc_sdk/trtc_cloud.dart';

import '../../common/logger/index.dart';

class LiveStreamService {
  late final TUIRoomEngine roomEngine;
  late final TUILiveListManager liveListManager;
  late final TUILiveLayoutManager liveLayoutManager;
  late final TRTCCloud? trtcCloud;

  LiveStreamService() {
    roomEngine = TUIRoomEngine.sharedInstance();
    liveListManager = roomEngine.getExtension(TUIExtensionType.liveListManager);
    liveLayoutManager = roomEngine.getExtension(TUIExtensionType.liveLayoutManager);
    _initTRTCCloud();
  }

  void addEngineObserver(TUIRoomObserver observer) {
    roomEngine.addObserver(observer);
  }

  void removeEngineObserver(TUIRoomObserver observer) {
    roomEngine.removeObserver(observer);
  }

  void addLiveListManagerObserver(TUILiveListObserver observer) {
    liveListManager.addObserver(observer);
  }

  void removeLiveListManagerObserver(TUILiveListObserver observer) {
    liveListManager.removeObserver(observer);
  }

  void addLiveLayoutObserver(TUILiveLayoutObserver observer) {
    liveLayoutManager.addObserver(observer);
  }

  void removeLiveLayoutObserver(TUILiveLayoutObserver observer) {
    liveLayoutManager.removeObserver(observer);
  }
}

extension LiveStreamServiceWithRoom on LiveStreamService {
  Future<TUIValueCallBack<TUILiveInfo>> fetchLiveInfo(String roomId) {
    return liveListManager.getLiveInfo(roomId);
  }
}

extension LiveStreamServiceWithUser on LiveStreamService {
  TUILoginUserInfo getSelfInfo() {
    return TUIRoomEngine.getSelfInfo();
  }

  Future<TUIValueCallBack<List<TUIUserInfo>>> getUserList() async {
    final List<TUIUserInfo> allUsers = [];
    int nextSequence = 0;
    while (true) {
      final result = await roomEngine.getUserList(nextSequence);
      if (result.code != TUIError.success || result.data == null) {
        return TUIValueCallBack(code: result.code, message: result.message);
      }
      TUIUserListResult userListResult = result.data!;
      allUsers.addAll(userListResult.userInfoList);
      nextSequence = userListResult.nextSequence;
      if (nextSequence == 0) {
        break;
      }
    }
    return TUIValueCallBack(code: TUIError.success, message: '', data: allUsers);
  }

  Future<TUIValueCallBack<TUIUserInfo>> getUserInfo(String userId) {
    return roomEngine.getUserInfo(userId);
  }

  Future<TUIActionCallback> disableSendingMessageByAdmin(String userId, bool isDisable) {
    return roomEngine.disableSendingMessageByAdmin(userId, isDisable);
  }

  Future<TUIActionCallback> kickRemoteUserOutOfRoom(String userId) {
    return roomEngine.kickRemoteUserOutOfRoom(userId);
  }
}

extension LiveStreamServiceWithMedia on LiveStreamService {
  void muteAllRemoteAudio(bool isMute) {
    trtcCloud?.muteAllRemoteAudio(isMute);
  }

  void setLocalVideoView(int viewId) {
    roomEngine.setLocalVideoView(viewId);
  }

  void enableGravitySensor(bool enable) {
    roomEngine.enableGravitySensor(enable);
  }

  void setVideoResolutionMode(TUIResolutionMode resolutionMode) {
    roomEngine.setVideoResolutionMode(TUIVideoStreamType.cameraStream, resolutionMode);
  }

  void updateVideoQuality(TUIVideoQuality videoQuality) {
    roomEngine.updateVideoQuality(videoQuality);
  }

  void updateVideoQualityEx(TUIVideoStreamType streamType, TUIRoomVideoEncoderParams params) {
    roomEngine.updateVideoQualityEx(streamType, params);
  }

  void enableMultiPlaybackQuality(bool enable) {
    try {
      Map<String, dynamic> params = {'enable': enable};

      Map<String, dynamic> jsonObject = {
        'api': 'enableMultiPlaybackQuality',
        'params': params,
      };

      String jsonString = jsonEncode(jsonObject);
      TUIRoomEngine.sharedInstance().invokeExperimentalAPI(jsonString);
    } catch (e) {
      LiveKitLogger.error('Error enableMultiPlaybackQuality');
    }
  }

  void enableSwitchPlaybackQuality(bool enable) async {
    Map<String, dynamic> config = {
      'key': 'Liteav.engine.set.live.qos.audience.strategy.version"',
      'value': enable ? 1 : 0
    };

    Map<String, dynamic> params = {
      'configs': [config]
    };
    Map<String, dynamic> jsonObject = {'api': 'setPrivateConfig', 'params': params};

    try {
      final jsonString = json.encode(jsonObject);
      final trtc = await TRTCCloud.sharedInstance();
      trtc.callExperimentalAPI(jsonString);
    } catch (e) {
      LiveKitLogger.error('Error enableSwitchPlaybackQuality');
    }
  }

  Future<TUIValueCallBack<List<TUIVideoQuality>>> queryPlaybackQualityList(String roomId) async {
    try {
      Map<String, dynamic> params = {'roomId': roomId};

      Map<String, dynamic> jsonObject = {
        'api': 'queryPlaybackQualityList',
        'params': params,
      };

      String jsonString = jsonEncode(jsonObject);
      final result = await TUIRoomEngine.sharedInstance().invokeExperimentalAPI(jsonString);
      if (result.code != TUIError.success || result.data == null) {
        return TUIValueCallBack(code: result.code, message: result.message);
      }
      return TUIValueCallBack(
          code: result.code, message: result.message, data: _decodeJsonString2TUIVideoQuality(result.data!));
    } catch (e) {
      LiveKitLogger.error('Error queryPlaybackQualityList');
      return TUIValueCallBack(code: TUIError.errFailed, message: 'queryPlaybackQualityList failed');
    }
  }

  void switchPlaybackQuality(TUIVideoQuality videoQuality) async {
    try {
      Map<String, dynamic> params = {'quality': videoQuality.value()};

      Map<String, dynamic> jsonObject = {
        'api': 'switchPlaybackQuality',
        'params': params,
      };

      String jsonString = jsonEncode(jsonObject);
      TUIRoomEngine.sharedInstance().invokeExperimentalAPI(jsonString);
    } catch (e) {
      LiveKitLogger.error('Error switchPlaybackQuality');
    }
  }

  void setAudioPlayoutVolume(int volume) {
    TUIRoomEngine.sharedInstance().setAudioPlayoutVolume(volume);
  }

  void pauseByAudience() async {
    Map<String, dynamic> params = {};
    Map<String, dynamic> jsonObject = {'api': 'pause', 'params': params};

    try {
      final jsonString = json.encode(jsonObject);
      final result = await TUIRoomEngine.sharedInstance().invokeExperimentalAPI(jsonString);
    } catch (e) {
      LiveKitLogger.error('Error pauseByAudience');
    }
  }

  void resumeByAudience() async {
    Map<String, dynamic> params = {};
    Map<String, dynamic> jsonObject = {'api': 'resume', 'params': params};

    try {
      final jsonString = json.encode(jsonObject);
      final result = await TUIRoomEngine.sharedInstance().invokeExperimentalAPI(jsonString);
    } catch (e) {
      LiveKitLogger.error('Error resumeByAudience');
    }
  }
}

extension LiveStreamServiceWithSeat on LiveStreamService {
  Future<TUIValueCallBack<List<TUISeatInfo>>> getSeatList() {
    return roomEngine.getSeatList();
  }

  Future<TUIValueCallBack<List<TUIRequest>>> getSeatApplicationList() {
    return roomEngine.getSeatApplicationList();
  }

  Future<TUIActionCallback> lockSeatByAdmin(int seatIndex, TUISeatLockParams lockParams) {
    return roomEngine.lockSeatByAdmin(seatIndex, lockParams);
  }
}

extension LiveStreamServiceWithCoHost on LiveStreamService {
  Future<TUIValueCallBack<TUILiveListResult>> fetchRecommendedList(String cursor, int count) {
    return liveListManager.fetchLiveList(cursor, count);
  }

  void setCoHostLayoutTemplateId(int templateId) {
    try {
      Map<String, dynamic> params = {'templateId': templateId};

      Map<String, dynamic> jsonObject = {'api': 'setCoHostLayoutTemplateId', 'params': params};

      final jsonString = jsonEncode(jsonObject);
      TUIRoomEngine.sharedInstance().invokeExperimentalAPI(jsonString);
    } catch (e) {
      LiveKitLogger.error('Error setCoHostLayoutTemplateId. templateId:$templateId');
    }
  }
}

extension on LiveStreamService {
  void _initTRTCCloud() async {
    trtcCloud = await TRTCCloud.sharedInstance();
  }

  List<TUIVideoQuality> _decodeJsonString2TUIVideoQuality(String jsonString) {
    try {
      final Map<String, dynamic> dataMap = jsonDecode(jsonString);
      final videoQualityNumbers = dataMap['data'];
      final List<TUIVideoQuality> videoQualities = [];
      for (int videoQualityNumber in videoQualityNumbers) {
        videoQualities.add(TUIVideoQualityExt.fromValue(videoQualityNumber));
      }
      return videoQualities;
    } catch (e) {
      return List.empty();
    }
  }
}
