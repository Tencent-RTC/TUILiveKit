import { LIVE_STAGE } from 'constants/room';
export default {
  // 直播间阶段
  liveStage: LIVE_STAGE.NOT_STARTED,
  // 房间号信息
  roomId: null,
  // 直播间昵称
  roomName: '',
  // 应用信息
  appInfo: {
    sdkAppId: 0,
    userSig: '',
    shareUserSig: '',
  },
  // 用户信息
  userInfo: {
    userId: '',
    userName: '',
    userAvatar: '',
  },
  // 是否在屏幕分享中
  isScreenSharing: false,
  // 使用的摄像头设备
  activeCamera: {},
  // 使用的麦克风设备
  activeMicrophone: {},
  // 使用的扬声器设备
  activeSpeaker: {},
  // 本地流是否设置镜像
  isSetMirror: true,
  // 视频参数
  videoProfile: {
    width: 1280,
    height: 720,
    frameRate: 15,
    bitrate: 2000,
  },
  // 是否开启美颜
  isOpenBeauty: false,
  // 美颜参数
  beautyParam: {
    beautyValue: 50,
    brightnessValue: 50,
    ruddyValue: 50,
  },
  // 屏幕分享参数
  screenProfile: {
    width: 1920,
    height: 1080,
    frameRate: 5,
    bitrate: 1600,
  },
  // 是否开启录制
  isRecordLive: true,
  // 音频采集状态
  isAudioMuted: false,
  // 视频采集状态
  isVideoMuted: false,
  // 音量大小
  audioLevel: 0,
  // 上行网络质量等级
  uplinkQualityLevel: 0,

  // 观看人员列表
  memberList: [],
};
