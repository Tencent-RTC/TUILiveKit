/*
 * @Description: vuex-getter
 * @Date: 2021-11-03 10:40:21
 * @LastEditTime: 2021-11-08 20:07:46
 */
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
};
