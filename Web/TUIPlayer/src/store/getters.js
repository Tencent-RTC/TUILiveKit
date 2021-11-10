export default {
  // 当前使用的摄像头设备Id
  activeCameraId(state) {
    return state.activeCamera.deviceId;
  },
  // 当前使用的摄像头设备Id
  activeMicrophoneId(state) {
    return state.activeMicrophone.deviceId;
  },
  // 当前使用的扬声器设备Id
  activeSpeakerId(state) {
    return state.activeSpeaker.deviceId;
  },
  // 音视频流是否正在播放中
  isPlaying(state) {
    return state.playState === 'playing';
  },
  // 音视频流是否正在暂停中
  isPaused(state) {
    return state.playState === 'paused';
  },
};
