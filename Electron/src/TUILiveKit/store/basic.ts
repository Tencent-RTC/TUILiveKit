import { defineStore } from "pinia";
import { TRTCAppScene, TRTCStatistics } from "trtc-electron-sdk";
import { useI18n } from '../locales/index';

const { t } = useI18n();

interface BasicState {
  sdkAppId: number;
  userId: string;
  userSig: string;
  userName: string;
  avatarUrl: string;
  useStringRoomId: boolean;
  roomId: string;
  isLiving: boolean;
  activeSettingTab: string;
  isOpenMic: boolean;
  statistics: Record<string, any>; // TRTCStatistics,
}

export const useBasicStore = defineStore('basic', {
  state: (): BasicState => ({
    sdkAppId: 0,
    userId: '',
    userSig: '',
    userName: '',
    avatarUrl: '',
    useStringRoomId: false,
    roomId: '',
    isLiving: false,
    activeSettingTab: 'audio',
    isOpenMic: false,
    statistics: {
      appCpu: 0,
      downLoss: 0,
      localStatisticsArray: [],
      localStatisticsArraySize: 0,
      receivedBytes: 0,
      remoteStatisticsArray: [],
      remoteStatisticsArraySize: 0,
      rtt: 0,
      sentBytes: 0,
      systemCpu: 0,
      upLoss: 0,
      appMemoryUsageInMB: 0,
    },
  }),
  getters: {
    localFrameRate: (state) => {
      return state.statistics.localStatisticsArray?.[0]?.frameRate;
    },
    roomName(): string {
      return t('sb. living room: NO.', {
        userName: this.userName || this.userId,
      }) + this.roomId;
    }
  },
  actions: {
    setSdkAppId(sdkAppId: number) {
      this.sdkAppId = sdkAppId;
    },
    setUserId(userId: string) {
      this.userId = userId;
    },
    setUserSig(userSig: string) {
      this.userSig = userSig;
    },
    setUserName(userName: string) {
      this.userName = userName;
    },
    setAvatarUrl(avatarUrl: string) {
      this.avatarUrl = avatarUrl;
    },
    setRoomId(roomId: string) {
      this.roomId = roomId;
      this.useStringRoomId = typeof roomId === 'string';
    },
    setBasicInfo(info: Record<string, any>) {
      if (!info) {
        return;
      }
      const { sdkAppId, userId, userSig, userName, avatarUrl, roomId } = info;
      sdkAppId && this.setSdkAppId(sdkAppId);
      userId && this.setUserId(userId);
      userSig && this.setUserSig(userSig);
      userName && this.setUserName(userName);
      avatarUrl && this.setAvatarUrl(avatarUrl);
      roomId && this.setRoomId(roomId);
    },
    setIsOpenMic(isOpen: boolean) {
      this.isOpenMic = isOpen;
    },
    setIsLiving(flag: boolean) {
      this.isLiving = flag;
    },
    setActiveSettingTab(tabName: string) {
      this.activeSettingTab = tabName;
    },
    setStatistics(statistics: Record<string, any>) {
      this.statistics = statistics;
    },
    reset() {
      this.roomId = '0';
      this.useStringRoomId = false;
      this.isLiving = false;
      this.activeSettingTab = 'audio';
      this.statistics = {
        appCpu: 0,
        downLoss: 0,
        localStatisticsArray: [],
        localStatisticsArraySize: 0,
        receivedBytes: 0,
        remoteStatisticsArray: [],
        remoteStatisticsArraySize: 0,
        rtt: 0,
        sentBytes: 0,
        systemCpu: 0,
        upLoss: 0,
        appMemoryUsageInMB: 0,
      };
    }
  }
});
