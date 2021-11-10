/*
 * @Description: vuex-state
 * @Date: 2021-11-04 11:02:45
 * @LastEditTime: 2021-11-08 20:00:52
 */
import { LIVE_STAGE } from 'constants/room';
export default {
  // sdkAppId
  sdkAppId: '',
  // userSig
  userSig: '',
  // 播放域名
  playerDomain: '',
  // 用户信息
  userInfo: {
    userId: '',
    userName: '',
    userAvatar: '',
  },
  // 主播用户id
  anchorUserId: '',
  // 房间号信息
  roomId: null,
  // 直播间昵称
  roomName: '',
  // 直播间阶段
  liveStage: LIVE_STAGE.NOT_STARTED,
  // 线路选择-rtc|cdn|快直播
  lineType: 'rtc',
  // 播放状态-playing|paused
  playState: 'paused',
};
