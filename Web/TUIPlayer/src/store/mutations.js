/*
 * @Description: vuex-mutations
 * @Date: 2021-11-04 11:02:45
 * @LastEditTime: 2022-02-15 18:06:33
 */
import * as mutationTypes from 'constants/mutation-types';

export default {
  // 设置sdkAppId
  [mutationTypes.SET_SDK_APP_ID](state, value) {
    state.sdkAppId = value;
  },
  // 设置userSig
  [mutationTypes.SET_USER_SIG](state, value) {
    state.userSig = value;
  },
  // 设置播放域名
  [mutationTypes.SET_PLAYER_DOMAIN](state, value) {
    state.playerDomain = value;
  },
  // 更新直播间ID
  [mutationTypes.SET_ROOM_ID](state, value) {
    state.roomId = value;
  },
  // 更新直播间昵称
  [mutationTypes.SET_ROOM_NAME](state, value) {
    state.roomName = value;
  },
  // 更新用户信息
  [mutationTypes.UPDATE_USER_INFO](state, data) {
    state.userInfo = data;
  },
  // 设置主播用户ID
  [mutationTypes.SET_ANCHOR_USER_ID](state, data) {
    state.anchorUserId = data;
  },
  // 更新直播间阶段
  [mutationTypes.UPDATE_LIVE_STAGE](state, value) {
    state.liveStage = value;
  },
  // 更新播放线路
  [mutationTypes.UPDATE_LINE_TYPE](state, value) {
    state.lineType = value;
  },
  // 更新流播放状态
  [mutationTypes.UPDATE_PLAY_STATE](state, value) {
    state.playState = value;
  },
  // 更新使用的摄像头设备
  [mutationTypes.UPDATE_ACTIVE_CAMERA](state, device) {
    state.activeCamera = device;
  },
  // 更新使用的麦克风设备
  [mutationTypes.UPDATE_ACTIVE_MICROPHONE](state, device) {
    state.activeMicrophone = device;
  },
  // 更新使用的扬声器设备
  [mutationTypes.UPDATE_ACTIVE_SPEAKER](state, device) {
    state.activeSpeaker = device;
  },
  // 更新是否设置本地流镜像
  [mutationTypes.UPDATE_SET_MIRROR](state, value) {
    state.isSetMirror = value;
  },
  // 更新是否设置本地流镜像
  [mutationTypes.UPDATE_OPEN_BEAUTY](state, value) {
    state.isOpenBeauty = value;
  },
  // 更新视频参数
  [mutationTypes.UPDATE_VIDEO_PROFILE](state, data) {
    state.videoProfile = data;
  },
  // 更新美颜参数
  [mutationTypes.UPDATE_BEAUTY_PARAM](state, data) {
    state.beautyParam = data;
  },
  // 更新是否录制直播
  [mutationTypes.UPDATE_RECORD_LIVE](state, value) {
    state.isRecordLive = value;
  },
  // 更新视频mute状态
  [mutationTypes.UPDATE_VIDEO_STATE](state, value) {
    state.isVideoMuted = value;
  },
  // 更新音频mute状态
  [mutationTypes.UPDATE_AUDIO_STATE](state, value) {
    state.isAudioMuted = value;
  },
  // 更新音量大小
  [mutationTypes.UPDATE_AUDIO_LEVEL](state, value) {
    state.audioLevel = value;
  },
  // 更新上行网络等级
  [mutationTypes.UPDATE_UPLINK_NETWORK_LEVEL](state, value) {
    state.uplinkQualityLevel = value;
  },
  // 更新横竖屏
  [mutationTypes.UPDATE_LAYOUT](state, value) {
    state.layout = value;
  },
  // 设置当前浏览器是否支持 webRTC
  [mutationTypes.SET_IS_SUPPORT_WEBRTC](state, value) {
    state.isSupportWebRTC = value;
  },
};
