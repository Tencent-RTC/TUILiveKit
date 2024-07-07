import { defineStore } from 'pinia';
import {
  TUIRole,
  TUIRoomInfo,
  TUISeatInfo,
  TUISeatMode,
  TUIUserInfo,
  TUIVideoQuality,
  TUIVideoStreamType,
  TUIMediaDeviceType,
} from '@tencentcloud/tuiroom-engine-electron';
import { useBasicStore } from './basic';
import { useChatStore } from './chat';
import { set, del } from '../utils/vue';
import useGetRoomEngine from '../utils/useRoomEngine';
import { messageChannels } from '../communication';
import { onError } from '../hooks/useErrorHandler';
import { useI18n } from '../locales';
const logger = console;
const logPrefix = '[roomStore]';
const { t } = useI18n();

const roomEngine = useGetRoomEngine();

export type UserInfo = {
    userId: string,
    userName?: string,
    avatarUrl?: string,
    userRole?: TUIRole,
    // Is it on the seat
    onSeat?: boolean,
    // Whether the user is applying for seat
    isUserApplyingToAnchor?: boolean,
    // The requestId of the user requesting to be on the seat
    applyToAnchorRequestId?: string,
    // The time at which a user applies to be on the seat
    applyToAnchorTimestamp?: number,
}

interface RoomState {
    localUser: UserInfo,
    remoteUserObj: Record<string, UserInfo>,
    masterUserId: string,
    canControlSelfAudio: boolean;
    canControlSelfVideo: boolean;
    isMicrophoneDisableForAllUser: boolean;
    isCameraDisableForAllUser: boolean;
    isMessageDisableForAllUser: boolean;
    isSeatEnabled: boolean;
    seatMode: TUISeatMode;
    maxSeatCount: number;
    roomName: string;
    historyRemoteUserCount: number;
}
export const useRoomStore = defineStore('room', {
  state: (): RoomState => ({
    localUser: {
      userId: '',
      userName: '',
      avatarUrl: '',
      userRole: TUIRole.kRoomOwner,
      onSeat: false,
    },
    remoteUserObj: {},
    masterUserId: '',
    canControlSelfAudio: true,
    canControlSelfVideo: true,
    isMicrophoneDisableForAllUser: false,
    isCameraDisableForAllUser: false,
    isMessageDisableForAllUser: false,
    isSeatEnabled: true,
    seatMode: TUISeatMode.kApplyToTake,
    maxSeatCount: 0,
    roomName: '',
    historyRemoteUserCount: 0,
  }),
  getters: {
    isMaster(state) {
      return state.localUser.userId === state.masterUserId;
    },
    remoteUserList(): Array<UserInfo> {
      return [...Object.values(this.remoteUserObj)];
    },
    applyToAnchorList: state => [...Object.values(state.remoteUserObj)]
      .filter(item => item.isUserApplyingToAnchor)
      .sort((item1, item2) => (item1?.applyToAnchorTimestamp || 0) - (item2?.applyToAnchorTimestamp || 0)) || [],
    anchorList: state => [...Object.values(state.remoteUserObj)].filter(item => item.onSeat),
  },
  actions: {
    setLocalUser(obj: Record<string, any>) {
      Object.assign(this.localUser, obj);
    },
    setRoomInfo(roomInfo: TUIRoomInfo) {
      const {
        roomOwner, isMicrophoneDisableForAllUser,
        isCameraDisableForAllUser, isMessageDisableForAllUser,
        isSeatEnabled, seatMode, maxSeatCount, roomName,
      } = roomInfo;
      if (this.localUser.userId === roomOwner) {
        this.localUser.userRole = TUIRole.kRoomOwner;
      }

      this.masterUserId = roomOwner;
      this.isMicrophoneDisableForAllUser = isMicrophoneDisableForAllUser;
      this.isCameraDisableForAllUser = isCameraDisableForAllUser;
      this.isMessageDisableForAllUser = isMessageDisableForAllUser;
      this.isSeatEnabled = isSeatEnabled;
      this.seatMode = seatMode;
      this.canControlSelfAudio = !this.isMicrophoneDisableForAllUser;
      this.canControlSelfVideo = !this.isCameraDisableForAllUser;
      this.maxSeatCount = maxSeatCount;
      this.roomName = roomName;
    },
    getNewUserInfo(userId: string) {
      const newUserInfo = {
        userId,
        userName: '',
        avatarUrl: '',
        onSeat: false,
        isUserApplyingToAnchor: false,
      };
      return newUserInfo;
    },
    addRemoteUser(userInfo: UserInfo) {
      const { userId, userName } = userInfo;
      if (this.remoteUserObj[userId]) {
        Object.assign(this.remoteUserObj[userId], userInfo);
      } else {
        const newUserInfo = Object.assign(this.getNewUserInfo(userId), userInfo);
        this.remoteUserObj[userId] = newUserInfo;
      }
      this.historyRemoteUserCount++;

      const chatStore = useChatStore();
      chatStore.updateMessageList({
        ID: `tui-custom-enter-room-${userId}`,
        type: 'CustomUserEnter',
        payload: {
          text: t('enter room'),
        },
        nick: userName || userId,
        from: userId, 
        flow: 'in',
        sequence: -1,
      });
    },
    removeRemoteUser(userId: string) {
      del(this.remoteUserObj, userId);
    },
    addApplyToAnchorUser(options: { userId: string, requestId: string, timestamp: number }) {
      const { userId, requestId, timestamp } = options;
      const remoteUserInfo = this.remoteUserObj[userId];
      if (remoteUserInfo) {
        remoteUserInfo.isUserApplyingToAnchor = true;
        remoteUserInfo.applyToAnchorRequestId = requestId;
        remoteUserInfo.applyToAnchorTimestamp = timestamp;
        remoteUserInfo.onSeat = false;
      }
      messageChannels.childWindowPort?.postMessage({
        key: "set-apply-list",
        data: JSON.stringify(this.applyToAnchorList),
      });
    },
    removeApplyToAnchorUser(userId: string) {
      const remoteUserInfo = this.remoteUserObj[userId];
      if (remoteUserInfo) {
        remoteUserInfo.isUserApplyingToAnchor = false;
        remoteUserInfo.applyToAnchorRequestId = '';
        remoteUserInfo.applyToAnchorTimestamp = 0;
        remoteUserInfo.onSeat = false;

        messageChannels.childWindowPort?.postMessage({
          key: "set-apply-list",
          data: JSON.stringify(this.applyToAnchorList),
        });
      }
    },
    async handleApplyToAnchorUser(userId: string, agree: boolean) {
      const remoteUserInfo = this.remoteUserObj[userId];
      if (remoteUserInfo) {
        try {
          const requestId = remoteUserInfo.applyToAnchorRequestId;
          if (requestId) {
            await roomEngine.instance?.responseRemoteRequest({
              requestId,
              agree,
            });
          }
          remoteUserInfo.isUserApplyingToAnchor = false;
          remoteUserInfo.applyToAnchorRequestId = '';
          remoteUserInfo.applyToAnchorTimestamp = 0;
          remoteUserInfo.onSeat = !!agree;

          messageChannels.childWindowPort?.postMessage({
            key: "set-apply-list",
            data: JSON.stringify(this.applyToAnchorList),
          });
        } catch (e: any) {
          onError(e);
        }
      }
    },
    async kickUserOffSeat(userId: string) {
      if (userId) {
        await roomEngine.instance?.kickUserOffSeatByAdmin({
          seatIndex: -1,
          userId
        });
      }
    },
    kickUserOutOfRoom(userId: string) {
      if (userId) {
        roomEngine.instance?.kickRemoteUserOutOfRoom({
          userId
        });
      }
    },
    // Updating changes to seatList
    // The onSeatListChanged, onUserVideoAvailable, onUserAudioAvailable events are notified as soon as the room is entered, so they are updated to the userMap first.
    // Wait for getUserList to get the full list of users before updating it.
    updateOnSeatList(seatedList: TUISeatInfo[], leftList: TUISeatInfo[]) {
      seatedList.forEach((seat) => {
        const { userId } = seat;
        if (userId === this.localUser.userId) {
          Object.assign(this.localUser, { onSeat: true });
        } else {
          const user = this.remoteUserObj[userId];
          if (user) {
            Object.assign(user, { onSeat: true });
          } else {
            const newUserInfo = Object.assign(this.getNewUserInfo(userId), { onSeat: true });
            set(this.remoteUserObj, userId, newUserInfo);
          }
        }
      });
      leftList.forEach((seat) => {
        if (seat.userId === this.localUser.userId) {
          Object.assign(this.localUser, { onSeat: false });
          const basicStore = useBasicStore();
          basicStore.setIsOpenMic(false);
        } else {
          const user = this.remoteUserObj[seat.userId];
          user && Object.assign(user, { onSeat: false });
        }
      });

      messageChannels.childWindowPort?.postMessage({
        key: "set-apply-list",
        data: JSON.stringify(this.applyToAnchorList),
      });
      messageChannels.childWindowPort?.postMessage({
        key: "set-anchor-list",
        data: JSON.stringify(this.anchorList),
      });
    },
    reset() {
      this.localUser = {
        userId: '',
        userName: '',
        avatarUrl: '',
        userRole: TUIRole.kRoomOwner,
        onSeat: false,
      };
      this.remoteUserObj = {};
      this.masterUserId = '';
      this.canControlSelfAudio = true;
      this.canControlSelfVideo = true;
      this.isMicrophoneDisableForAllUser = false;
      this.isCameraDisableForAllUser = false;
      this.isMessageDisableForAllUser = false;
      this.isSeatEnabled = true;
      this.seatMode = TUISeatMode.kApplyToTake;
      this.maxSeatCount = 0;
      this.roomName = '';
      this.historyRemoteUserCount = 0;
    }
  }
})