import 'package:rtc_room_engine/api/extension/tui_live_list_manager.dart';
import 'package:rtc_room_engine/impl/tui_room_engine_impl.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/manager/v2_tim_friendship_manager.dart';
import 'package:tencent_cloud_chat_sdk/manager/v2_tim_manager.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_info.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_operation_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_type_check_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_value_callback.dart';
import 'package:tencent_live_uikit/service/live_service.dart';
import 'package:tencent_trtc_cloud/trtc_cloud.dart';
import 'package:tencent_trtc_cloud/trtc_cloud_def.dart';
import 'package:tencent_trtc_cloud/tx_audio_effect_manager.dart';
import 'package:tencent_trtc_cloud/tx_beauty_manager.dart';
import 'package:tencent_trtc_cloud/tx_device_manager.dart';

class RoomEngineService implements ILiveService {
  late final TUIRoomEngine roomEngine;
  late final TUILiveListManager liveListManager;
  late final TUIRoomDeviceManager roomDeviceManager;
  late final TRTCCloud? trtcCloud;
  late final TXAudioEffectManager? audioEffectManager;
  late final TXDeviceManager? deviceManager;
  late final TXBeautyManager? beautyManager;
  late final V2TIMManager imManager;
  late final V2TIMFriendshipManager friendshipManager;

  RoomEngineService() {
    _createTRTCCloud();
    _createTUIRoomEngine();
    _createIMManager();
  }

  @override
  void addObserver(TUIRoomObserver observer) {
    roomEngine.addObserver(observer);
  }

  @override
  void callExperimentalAPI(String jsonStr) {
    TUIRoomEngine.callExperimentalAPI(jsonStr);
  }

  @override
  Future<TUIActionCallback> cancelRequest(String requestId) {
    return roomEngine.cancelRequest(requestId);
  }

  @override
  Future<V2TimValueCallback<List<V2TimFollowTypeCheckResult>>> checkFollowType({required List<String> userIDList}) {
    return friendshipManager.checkFollowType(userIDList: userIDList);
  }

  @override
  void closeLocalCamera() {
    return roomEngine.closeLocalCamera();
  }

  @override
  void destroy() {
    _destroyTRTCCloud();
    _destroyTUIRoomEngine();
  }

  @override
  void enableGravitySensor(bool enable) {
    return roomEngine.enableGravitySensor(enable);
  }

  @override
  Future<void> enableVoiceEarMonitor(bool enable) async {
    return await audioEffectManager?.enableVoiceEarMonitor(enable);
  }

  @override
  Future<TUIValueCallBack<TUILiveListResult>> fetchLiveList(String cursor, int count) {
    return liveListManager.fetchLiveList(cursor, count);
  }

  @override
  Future<V2TimValueCallback<List<V2TimFollowOperationResult>>> followUser({required List<String> userIDList}) {
    return friendshipManager.followUser(userIDList: userIDList);
  }

  @override
  Future<TUIValueCallBack<List<TUIRequest>>> getSeatApplicationList() {
    return roomEngine.getSeatApplicationList();
  }

  @override
  Future<TUIValueCallBack<List<TUISeatInfo>>> getSeatList() {
    return roomEngine.getSeatList();
  }

  @override
  Future<V2TimValueCallback<List<V2TimFollowInfo>>> getUserFollowInfo({required List<String> userIDList}) {
    return friendshipManager.getUserFollowInfo(userIDList: userIDList);
  }

  @override
  Future<TUIValueCallBack<TUIUserInfo>> getUserInfo(String userId) {
    return roomEngine.getUserInfo(userId);
  }

  @override
  Future<TUIValueCallBack<TUIUserListResult>> getUserList(int nextSequence) {
    return roomEngine.getUserList(nextSequence);
  }

  @override
  Future<TUIValueCallBack<TUIRoomInfo>> join(String roomId) {
    return roomEngine.enterRoom(roomId, roomType: TUIRoomType.livingRoom);
  }

  @override
  Future<TUIActionCallback> kickUserOffSeatByAdmin(int seatIndex, String userId) {
    return roomEngine.kickUserOffSeatByAdmin(seatIndex, userId);
  }

  @override
  Future<TUIActionCallback> leave() {
    return roomEngine.exitRoom(true);
  }

  @override
  Future<TUIActionCallback> leaveSeat() {
    return roomEngine.leaveSeat();
  }

  @override
  Future<TUIActionCallback> lockSeatByAdmin(int seatIndex, TUISeatLockParams lockParams) {
    return roomEngine.lockSeatByAdmin(seatIndex, lockParams);
  }

  @override
  Future<void> muteAllRemoteAudio(bool mute) async {
    return await trtcCloud?.muteAllRemoteAudio(mute);
  }

  @override
  Future<TUIActionCallback> muteLocalAudio() {
    return roomEngine.muteLocalAudio();
  }

  @override
  Future<TUIActionCallback> openLocalCamera(bool isFront, TUIVideoQuality quality) {
    return roomEngine.openLocalCamera(isFront, quality);
  }

  @override
  Future<TUIActionCallback> openLocalMicrophone(TUIAudioQuality quality) {
    return roomEngine.openLocalMicrophone(quality);
  }

  @override
  void removeObserver(TUIRoomObserver observer) {
    return roomEngine.removeObserver(observer);
  }

  @override
  Future<TUIActionCallback> responseRemoteRequest(String requestId, bool agree) {
    return roomEngine.responseRemoteRequest(requestId, agree);
  }

  @override
  Future<void> setAllMusicVolume(int volume) async {
    return await audioEffectManager?.setAllMusicVolume(volume);
  }

  @override
  Future<void> setBeautyLevel(int beautyLevel) async {
    return await beautyManager?.setBeautyLevel(beautyLevel);
  }

  @override
  Future<void> setBeautyStyle(int beautyStyle) async {
    return await beautyManager?.setBeautyStyle(beautyStyle);
  }

  @override
  void setCameraMirror(bool isMirror) async {
    TRTCRenderParams trtcRenderParams = TRTCRenderParams();
    trtcRenderParams.mirrorType =
        isMirror ? TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_ENABLE : TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_DISABLE;

    await trtcCloud?.setLocalRenderParams(trtcRenderParams);
    await trtcCloud?.setVideoEncoderMirror(isMirror);
  }

  @override
  Future<TUIActionCallback> setLiveInfo(
      String roomId, {String? coverUrl, List<int>? categoryList, bool? isPublicVisible, int? activityStatus}) {
    return liveListManager.setLiveInfo(roomId,
        coverUrl: coverUrl,
        categoryList: categoryList,
        isPublicVisible: isPublicVisible,
        activityStatus: activityStatus);
  }

  @override
  void setLocalVideoView(int viewId) {
    return roomEngine.setLocalVideoView(viewId);
  }

  @override
  void setRemoteVideoView(String userId, TUIVideoStreamType streamType, int viewId) {
    return roomEngine.setRemoteVideoView(userId, streamType, viewId);
  }

  @override
  Future<void> setRuddyLevel(int ruddyLevel) async {
    return await beautyManager?.setBeautyLevel(ruddyLevel);
  }

  @override
  void setVideoResolutionMode(TUIVideoStreamType streamType, TUIResolutionMode resolutionMode) {
    return roomEngine.setVideoResolutionMode(streamType, resolutionMode);
  }

  @override
  Future<void> setVoiceCaptureVolume(int volume) async {
    return await audioEffectManager?.setVoiceCaptureVolume(volume);
  }

  @override
  Future<void> setVoiceChangerType(int type) async {
    return await audioEffectManager?.setVoiceChangerType(type);
  }

  @override
  Future<void> setVoiceEarMonitorVolume(int volume) async {
    return await audioEffectManager?.setVoiceEarMonitorVolume(volume);
  }

  @override
  Future<void> setVoiceReverbType(int type) async {
    return await audioEffectManager?.setVoiceReverbType(type);
  }

  @override
  Future<void> setWhitenessLevel(int whitenessLevel) async {
    return await beautyManager?.setWhitenessLevel(whitenessLevel);
  }

  @override
  Future<TUIValueCallBack<TUIRoomInfo>> start(TUIRoomInfo roomInfo) async {
    final result = await roomEngine.createRoom(roomInfo);
    if (result.code == TUIError.success) {
      return roomEngine.enterRoom(roomInfo.roomId, roomType: TUIRoomType.livingRoom);
    }
    return TUIValueCallBack(code: TUIError.errFailed, message: "failed");
  }

  @override
  Future<bool?> startPlayMusic(AudioMusicParam musicParam) async {
    return await audioEffectManager?.startPlayMusic(musicParam);
  }

  @override
  void startPlayRemoteVideo(String userId, TUIVideoStreamType streamType, TUIPlayCallback? playCallback) {
    return roomEngine.startPlayRemoteVideo(userId, streamType, playCallback);
  }

  @override
  Future<TUIActionCallback> stop() {
    return roomEngine.destroyRoom();
  }

  @override
  Future<void> stopPlayMusic(int id) async {
    return await audioEffectManager?.stopPlayMusic(id);
  }

  @override
  Future<int?> switchCamera(bool frontCamera) async {
    return await deviceManager?.switchCamera(frontCamera);
  }

  @override
  TUIRequest takeSeat(int seatIndex, int timeout, TUIRequestCallback? requestCallback) {
    return roomEngine.takeSeat(seatIndex, timeout, requestCallback);
  }

  @override
  Future<TUIActionCallback> unMuteLocalAudio() {
    return roomEngine.unMuteLocalAudio();
  }

  @override
  Future<V2TimValueCallback<List<V2TimFollowOperationResult>>> unfollowUser({required List<String> userIDList}) {
    return friendshipManager.unfollowUser(userIDList: userIDList);
  }

  @override
  void updateAudioQuality(TUIAudioQuality quality) {
    return roomEngine.updateAudioQuality(quality);
  }

  @override
  void updateVideoQuality(TUIVideoQuality quality) {
    return roomEngine.updateVideoQuality(quality);
  }

  @override
  TUILoginUserInfo getSelfInfo() {
    return TUIRoomEngineImpl.getSelfInfo();
  }

  _createTRTCCloud() async {
    trtcCloud = await TRTCCloud.sharedInstance();
    beautyManager = trtcCloud?.getBeautyManager();
    audioEffectManager = trtcCloud?.getAudioEffectManager();
    deviceManager = trtcCloud?.getDeviceManager();
  }

  _createTUIRoomEngine() {
    roomEngine = TUIRoomEngine.sharedInstance();
    liveListManager = roomEngine.getExtension(TUIExtensionType.liveListManger);
    roomDeviceManager = roomEngine.getExtension(TUIExtensionType.deviceManager);
  }

  _createIMManager() {
    imManager = V2TIMManager();
    friendshipManager = imManager.getFriendshipManager();
  }

  _destroyTRTCCloud() async {
    await TRTCCloud.destroySharedInstance();
  }

  _destroyTUIRoomEngine() {
    TUIRoomEngine.destroySharedInstance();
  }
}
