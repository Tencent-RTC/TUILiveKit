import 'package:rtc_room_engine/impl/tui_room_engine_impl.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/manager/v2_tim_friendship_manager.dart';
import 'package:tencent_cloud_chat_sdk/manager/v2_tim_manager.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_info.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_operation_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_type_check_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_value_callback.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_trtc_cloud/trtc_cloud.dart';
import 'package:tencent_trtc_cloud/trtc_cloud_def.dart';
import 'package:tencent_trtc_cloud/tx_audio_effect_manager.dart';
import 'package:tencent_trtc_cloud/tx_beauty_manager.dart';
import 'package:tencent_trtc_cloud/tx_device_manager.dart';

import '../index.dart';

class LiveServiceImpl implements ILiveService {
  static const String tag = "LiveServiceImpl";

  late final TUIRoomEngine roomEngine;
  late final TUILiveListManager liveListManager;
  late final TUIRoomDeviceManager roomDeviceManager;
  late final TRTCCloud? trtcCloud;
  late final TXAudioEffectManager? audioEffectManager;
  late final TXDeviceManager? deviceManager;
  late final TXBeautyManager? beautyManager;
  late final V2TIMManager imManager;
  late final V2TIMFriendshipManager friendshipManager;

  LiveServiceImpl() {
    _createTRTCCloud();
    _createTUIRoomEngine();
    _createIMManager();
  }

  @override
  void addObserver(TUIRoomObserver observer) {
    LiveKitLogger.info("$tag($hashCode) addObserver:[observer:${observer.hashCode}]");
    roomEngine.addObserver(observer);
  }

  @override
  void callExperimentalAPI(String jsonStr) {
    LiveKitLogger.info("$tag($hashCode) callExperimentalAPI:[jsonStr:$jsonStr]");
    TUIRoomEngine.callExperimentalAPI(jsonStr);
  }

  @override
  Future<TUIActionCallback> cancelRequest(String requestId) {
    LiveKitLogger.info("$tag($hashCode) cancelRequest:[requestId:$requestId]");
    return roomEngine.cancelRequest(requestId);
  }

  @override
  Future<V2TimValueCallback<List<V2TimFollowTypeCheckResult>>> checkFollowType({required List<String> userIDList}) {
    LiveKitLogger.info("$tag($hashCode) checkFollowType:[userIDList:$userIDList]");
    return friendshipManager.checkFollowType(userIDList: userIDList);
  }

  @override
  void closeLocalCamera() {
    LiveKitLogger.info("$tag($hashCode) closeLocalCamera:[]");
    return roomEngine.closeLocalCamera();
  }

  @override
  void destroy() {
    LiveKitLogger.info("$tag($hashCode) destroy:[]");
  }

  @override
  void enableGravitySensor(bool enable) {
    LiveKitLogger.info("$tag($hashCode) enableGravitySensor:[enable:$enable]");
    return roomEngine.enableGravitySensor(enable);
  }

  @override
  Future<void> enableVoiceEarMonitor(bool enable) async {
    LiveKitLogger.info("$tag($hashCode) enableVoiceEarMonitor:[enable:$enable]");
    return await audioEffectManager?.enableVoiceEarMonitor(enable);
  }

  @override
  Future<TUIValueCallBack<TUILiveListResult>> fetchLiveList(String cursor, int count) {
    LiveKitLogger.info("$tag($hashCode) fetchLiveList:[cursor:$cursor,count:$count]");
    return liveListManager.fetchLiveList(cursor, count);
  }

  @override
  Future<V2TimValueCallback<List<V2TimFollowOperationResult>>> followUser({required List<String> userIDList}) {
    LiveKitLogger.info("$tag($hashCode) followUser:[userIDList:$userIDList]");
    return friendshipManager.followUser(userIDList: userIDList);
  }

  @override
  Future<TUIValueCallBack<List<TUIRequest>>> getSeatApplicationList() {
    LiveKitLogger.info("$tag($hashCode) getSeatApplicationList:[]");
    return roomEngine.getSeatApplicationList();
  }

  @override
  Future<TUIValueCallBack<List<TUISeatInfo>>> getSeatList() {
    LiveKitLogger.info("$tag($hashCode) getSeatList:[]");
    return roomEngine.getSeatList();
  }

  @override
  Future<V2TimValueCallback<List<V2TimFollowInfo>>> getUserFollowInfo({required List<String> userIDList}) {
    LiveKitLogger.info("$tag($hashCode) getUserFollowInfo:[userIDList:$userIDList]");
    return friendshipManager.getUserFollowInfo(userIDList: userIDList);
  }

  @override
  Future<TUIValueCallBack<TUIUserInfo>> getUserInfo(String userId) {
    LiveKitLogger.info("$tag($hashCode) getUserInfo:[userId:$userId]");
    return roomEngine.getUserInfo(userId);
  }

  @override
  Future<TUIValueCallBack<TUIUserListResult>> getUserList(int nextSequence) {
    LiveKitLogger.info("$tag($hashCode) getUserList:[nextSequence:$nextSequence]");
    return roomEngine.getUserList(nextSequence);
  }

  @override
  Future<TUIValueCallBack<TUIRoomInfo>> join(String roomId) {
    LiveKitLogger.info("$tag($hashCode) enterRoom:[roomId:$roomId,roomType:TUIRoomType.livingRoom]");
    return roomEngine.enterRoom(roomId, roomType: TUIRoomType.livingRoom);
  }

  @override
  Future<TUIActionCallback> kickUserOffSeatByAdmin(int seatIndex, String userId) {
    LiveKitLogger.info("$tag($hashCode) kickUserOffSeatByAdmin:[seatIndex:$seatIndex,userId:$userId]");
    return roomEngine.kickUserOffSeatByAdmin(seatIndex, userId);
  }

  @override
  Future<TUIActionCallback> leave() {
    LiveKitLogger.info("$tag($hashCode) exitRoom:[syncWaiting:true]");
    return roomEngine.exitRoom(true);
  }

  @override
  Future<TUIActionCallback> leaveSeat() {
    LiveKitLogger.info("$tag($hashCode) leaveSeat:[]");
    return roomEngine.leaveSeat();
  }

  @override
  Future<TUIActionCallback> lockSeatByAdmin(int seatIndex, TUISeatLockParams lockParams) {
    LiveKitLogger.info("$tag($hashCode) lockSeatByAdmin:[seatIndex:$seatIndex,lockParams:$lockParams]");
    return roomEngine.lockSeatByAdmin(seatIndex, lockParams);
  }

  @override
  Future<void> muteAllRemoteAudio(bool mute) async {
    LiveKitLogger.info("$tag($hashCode) muteAllRemoteAudio:[mute:$mute]");
    return await trtcCloud?.muteAllRemoteAudio(mute);
  }

  @override
  Future<TUIActionCallback> muteLocalAudio() {
    LiveKitLogger.info("$tag($hashCode) muteLocalAudio:[]");
    return roomEngine.muteLocalAudio();
  }

  @override
  Future<TUIActionCallback> openLocalCamera(bool isFront, TUIVideoQuality quality) {
    LiveKitLogger.info("$tag($hashCode) openLocalCamera:[isFront:$isFront,quality:$quality]");
    return roomEngine.openLocalCamera(isFront, quality);
  }

  @override
  Future<TUIActionCallback> openLocalMicrophone(TUIAudioQuality quality) {
    LiveKitLogger.info("$tag($hashCode) openLocalMicrophone:[quality:$quality]");
    return roomEngine.openLocalMicrophone(quality);
  }

  @override
  void removeObserver(TUIRoomObserver observer) {
    LiveKitLogger.info("$tag($hashCode) removeObserver:[observer:${observer.hashCode}]");
    return roomEngine.removeObserver(observer);
  }

  @override
  Future<TUIActionCallback> responseRemoteRequest(String requestId, bool agree) {
    LiveKitLogger.info("$tag($hashCode) responseRemoteRequest:[requestId:$requestId,agree:$agree]");
    return roomEngine.responseRemoteRequest(requestId, agree);
  }

  @override
  Future<void> setAllMusicVolume(int volume) async {
    LiveKitLogger.info("$tag($hashCode) setAllMusicVolume:[volume:$volume]");
    return await audioEffectManager?.setAllMusicVolume(volume);
  }

  @override
  Future<void> setBeautyLevel(int beautyLevel) async {
    LiveKitLogger.info("$tag($hashCode) setBeautyLevel:[beautyLevel:$beautyLevel]");
    return await beautyManager?.setBeautyLevel(beautyLevel);
  }

  @override
  Future<void> setBeautyStyle(int beautyStyle) async {
    LiveKitLogger.info("$tag($hashCode) setBeautyStyle:[beautyStyle:$beautyStyle]");
    return await beautyManager?.setBeautyStyle(beautyStyle);
  }

  @override
  void setCameraMirror(bool isMirror) async {
    TRTCRenderParams trtcRenderParams = TRTCRenderParams();
    trtcRenderParams.mirrorType =
        isMirror ? TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_ENABLE : TRTCCloudDef.TRTC_VIDEO_MIRROR_TYPE_DISABLE;
    LiveKitLogger.info("$tag($hashCode) setLocalRenderParams:[trtcRenderParams:$trtcRenderParams]");
    LiveKitLogger.info("$tag($hashCode) setVideoEncoderMirror:[isMirror:$isMirror]");
    await trtcCloud?.setLocalRenderParams(trtcRenderParams);
    await trtcCloud?.setVideoEncoderMirror(isMirror);
  }

  @override
  Future<TUIActionCallback> setLiveInfo(String roomId,
      {String? coverUrl, List<int>? categoryList, bool? isPublicVisible, int? activityStatus}) {
    LiveKitLogger.info("$tag($hashCode) setLiveInfo:[roomId:$roomId,coverUrl:$coverUrl"
        ",categoryList:$categoryList,isPublicVisible:$isPublicVisible,activityStatus:$activityStatus]");
    return liveListManager.setLiveInfo(roomId,
        coverUrl: coverUrl,
        categoryList: categoryList,
        isPublicVisible: isPublicVisible,
        activityStatus: activityStatus);
  }

  @override
  void setLocalVideoView(int viewId) {
    LiveKitLogger.info("$tag($hashCode) setLocalVideoView:[viewId:$viewId]");
    return roomEngine.setLocalVideoView(viewId);
  }

  @override
  void setRemoteVideoView(String userId, TUIVideoStreamType streamType, int viewId) {
    LiveKitLogger.info("$tag($hashCode) setRemoteVideoView:[userId:$userId,streamType:$streamType,viewId:$viewId]");
    return roomEngine.setRemoteVideoView(userId, streamType, viewId);
  }

  @override
  Future<void> setRuddyLevel(int ruddyLevel) async {
    LiveKitLogger.info("$tag($hashCode) setBeautyLevel:[ruddyLevel:$ruddyLevel]");
    return await beautyManager?.setBeautyLevel(ruddyLevel);
  }

  @override
  void setVideoResolutionMode(TUIVideoStreamType streamType, TUIResolutionMode resolutionMode) {
    LiveKitLogger.info(
        "$tag($hashCode) setVideoResolutionMode:[streamType:$streamType,resolutionMode:$resolutionMode]");
    return roomEngine.setVideoResolutionMode(streamType, resolutionMode);
  }

  @override
  Future<void> setVoiceCaptureVolume(int volume) async {
    LiveKitLogger.info("$tag($hashCode) setVoiceCaptureVolume:[volume:$volume]");
    return await audioEffectManager?.setVoiceCaptureVolume(volume);
  }

  @override
  Future<void> setVoiceChangerType(int type) async {
    LiveKitLogger.info("$tag($hashCode) setVoiceChangerType:[type:$type]");
    return await audioEffectManager?.setVoiceChangerType(type);
  }

  @override
  Future<void> setVoiceEarMonitorVolume(int volume) async {
    LiveKitLogger.info("$tag($hashCode) setVoiceEarMonitorVolume:[volume:$volume]");
    return await audioEffectManager?.setVoiceEarMonitorVolume(volume);
  }

  @override
  Future<void> setVoiceReverbType(int type) async {
    LiveKitLogger.info("$tag($hashCode) setVoiceReverbType:[type:$type]");
    return await audioEffectManager?.setVoiceReverbType(type);
  }

  @override
  Future<void> setWhitenessLevel(int whitenessLevel) async {
    LiveKitLogger.info("$tag($hashCode) setWhitenessLevel:[whitenessLevel:$whitenessLevel]");
    return await beautyManager?.setWhitenessLevel(whitenessLevel);
  }

  @override
  Future<TUIValueCallBack<TUIRoomInfo>> start(TUIRoomInfo roomInfo) async {
    LiveKitLogger.info("$tag($hashCode) createRoom:[roomInfo:${roomInfo.roomId}]");
    final result = await roomEngine.createRoom(roomInfo);
    if (result.code == TUIError.success) {
      LiveKitLogger.info("$tag($hashCode) enterRoom:[roomId:${roomInfo.roomId},roomType:TUIRoomType.livingRoom]");
      return roomEngine.enterRoom(roomInfo.roomId, roomType: TUIRoomType.livingRoom);
    }
    return TUIValueCallBack(code: TUIError.errFailed, message: "failed");
  }

  @override
  Future<bool?> startPlayMusic(AudioMusicParam musicParam) async {
    LiveKitLogger.info("$tag($hashCode) startPlayMusic:[musicParam:$musicParam]");
    return await audioEffectManager?.startPlayMusic(musicParam);
  }

  @override
  void startPlayRemoteVideo(String userId, TUIVideoStreamType streamType, TUIPlayCallback? playCallback) {
    LiveKitLogger.info("$tag($hashCode) startPlayRemoteVideo:[userId:$userId,streamType:$streamType]");
    return roomEngine.startPlayRemoteVideo(userId, streamType, playCallback);
  }

  @override
  void stopPlayRemoteVideo(String userId, TUIVideoStreamType streamType) {
    LiveKitLogger.info("$tag($hashCode) stopPlayRemoteVideo:[userId:$userId,streamType:$streamType]");
    return roomEngine.stopPlayRemoteVideo(userId, streamType);
  }

  @override
  Future<TUIActionCallback> stop() {
    LiveKitLogger.info("$tag($hashCode) destroyRoom:[]");
    return roomEngine.destroyRoom();
  }

  @override
  Future<void> stopPlayMusic(int id) async {
    LiveKitLogger.info("$tag($hashCode) stopPlayMusic:[id:$id]");
    return await audioEffectManager?.stopPlayMusic(id);
  }

  @override
  Future<int?> switchCamera(bool frontCamera) async {
    LiveKitLogger.info("$tag($hashCode) switchCamera:[frontCamera:$frontCamera]");
    return await deviceManager?.switchCamera(frontCamera);
  }

  @override
  TUIRequest takeSeat(int seatIndex, int timeout, TUIRequestCallback? requestCallback) {
    LiveKitLogger.info("$tag($hashCode) takeSeat:[seatIndex:$seatIndex,timeout:$timeout]");
    return roomEngine.takeSeat(seatIndex, timeout, requestCallback);
  }

  @override
  Future<TUIActionCallback> unMuteLocalAudio() {
    LiveKitLogger.info("$tag($hashCode) unMuteLocalAudio:[]");
    return roomEngine.unMuteLocalAudio();
  }

  @override
  Future<V2TimValueCallback<List<V2TimFollowOperationResult>>> unfollowUser({required List<String> userIDList}) {
    LiveKitLogger.info("$tag($hashCode) unfollowUser:[userIDList:$userIDList]");
    return friendshipManager.unfollowUser(userIDList: userIDList);
  }

  @override
  void updateAudioQuality(TUIAudioQuality quality) {
    LiveKitLogger.info("$tag($hashCode) updateAudioQuality:[quality:$quality]");
    return roomEngine.updateAudioQuality(quality);
  }

  @override
  void updateVideoQuality(TUIVideoQuality quality) {
    LiveKitLogger.info("$tag($hashCode) updateVideoQuality:[quality:$quality]");
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
    liveListManager = roomEngine.getExtension(TUIExtensionType.liveListManager);
    roomDeviceManager = roomEngine.getExtension(TUIExtensionType.deviceManager);
  }

  _createIMManager() {
    imManager = V2TIMManager();
    friendshipManager = imManager.getFriendshipManager();
  }
}
