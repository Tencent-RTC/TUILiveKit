import * as mutationTypes from 'constants/mutation-types';

export default {
  // 更新直播间ID
  [mutationTypes.SET_ROOM_ID](state, value) {
    state.roomId = value;
  },
  // 更新直播间昵称
  [mutationTypes.UPDATE_ROOM_NAME](state, value) {
    state.roomName = value;
  },
  // 更新用户信息
  [mutationTypes.UPDATE_USER_INFO](state, data) {
    state.userInfo = data;
  },
  // 设置sdkAppId信息
  [mutationTypes.SET_APP_INFO](state, data) {
    state.appInfo = data;
  },
  // 更新直播间阶段
  [mutationTypes.UPDATE_LIVE_STAGE](state, value) {
    state.liveStage = value;
  },
  // 更新是否在屏幕分享中
  [mutationTypes.UPDATE_IS_SCREEN_SHARING](state, value) {
    state.isScreenSharing = value;
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
  // 更新直播间人员列表
  [mutationTypes.UPDATE_MEMBER_LIST](state, value) {
    state.memberList = value;
  },
};
