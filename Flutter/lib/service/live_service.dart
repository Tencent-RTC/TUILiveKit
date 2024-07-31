import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_info.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_operation_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_type_check_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_value_callback.dart';
import 'package:tencent_trtc_cloud/trtc_cloud_def.dart';

abstract class ILiveService {

  void destroy();

  void addObserver(TUIRoomObserver observer);

  void removeObserver(TUIRoomObserver observer);

  // Room Business
  Future<TUIValueCallBack<TUIRoomInfo>> start(TUIRoomInfo roomInfo);

  Future<TUIValueCallBack<TUIRoomInfo>> join(String roomId);

  Future<TUIActionCallback> leave();

  Future<TUIActionCallback> stop();

  // Seat Business
  TUIRequest takeSeat(int seatIndex, int timeout, TUIRequestCallback? requestCallback);

  Future<TUIActionCallback> leaveSeat();

  Future<TUIActionCallback> lockSeatByAdmin(int seatIndex, TUISeatLockParams lockParams);

  Future<TUIActionCallback> kickUserOffSeatByAdmin(int seatIndex, String userId);

  Future<TUIValueCallBack<List<TUISeatInfo>>> getSeatList();

  Future<TUIValueCallBack<List<TUIRequest>>> getSeatApplicationList();

  Future<TUIActionCallback> responseRemoteRequest(String requestId, bool agree);

  Future<TUIActionCallback> cancelRequest(String requestId);

  // User Business
  Future<TUIValueCallBack<TUIUserListResult>> getUserList(int nextSequence);

  Future<TUIValueCallBack<TUIUserInfo>> getUserInfo(String userId);

  Future<void> muteAllRemoteAudio(bool mute);

  // Media Business
  Future<TUIActionCallback> openLocalMicrophone(TUIAudioQuality quality);

  Future<TUIActionCallback> openLocalCamera(bool isFront, TUIVideoQuality quality);

  void closeLocalCamera();

  void setCameraMirror(bool isMirror);

  Future<int?> switchCamera(bool frontCamera);

  void setLocalVideoView(int viewId);

  void setRemoteVideoView(String userId, TUIVideoStreamType streamType, int viewId);

  void startPlayRemoteVideo(String userId, TUIVideoStreamType streamType, TUIPlayCallback? playCallback);

  Future<TUIActionCallback> muteLocalAudio();

  Future<TUIActionCallback> unMuteLocalAudio();

  void enableGravitySensor(bool enable);

  void updateVideoQuality(TUIVideoQuality quality);

  void updateAudioQuality(TUIAudioQuality quality);

  void setVideoResolutionMode(TUIVideoStreamType streamType, TUIResolutionMode resolutionMode);

  Future<void> setVoiceChangerType(int type);

  Future<void> setVoiceReverbType(int type);

  Future<void> enableVoiceEarMonitor(bool enable);

  Future<void> stopPlayMusic(int id);

  Future<bool?> startPlayMusic(AudioMusicParam musicParam);

  Future<void> setAllMusicVolume(int volume);

  Future<void> setVoiceEarMonitorVolume(int volume);

  Future<void> setVoiceCaptureVolume(int volume);

  Future<void> setBeautyStyle(int beautyStyle);

  Future<void> setBeautyLevel(int beautyLevel);

  Future<void> setWhitenessLevel(int whitenessLevel) ;

  Future<void> setRuddyLevel(int ruddyLevel);

  // IM Business
  Future<V2TimValueCallback<List<V2TimFollowOperationResult>>> followUser({required List<String> userIDList});

  Future<V2TimValueCallback<List<V2TimFollowOperationResult>>> unfollowUser({required List<String> userIDList});

  Future<V2TimValueCallback<List<V2TimFollowTypeCheckResult>>> checkFollowType({required List<String> userIDList});

  Future<V2TimValueCallback<List<V2TimFollowInfo>>> getUserFollowInfo({required List<String> userIDList});

  // Data Report
  void callExperimentalAPI(String jsonStr);

  Future<TUIValueCallBack<TUILiveListResult>> fetchLiveList(String cursor, int count);
  Future<TUIActionCallback> setLiveInfo(
      String roomId, {String? coverUrl, List<int>? categoryList, bool? isPublicVisible, int? activityStatus});

  TUILoginUserInfo getSelfInfo();
}
