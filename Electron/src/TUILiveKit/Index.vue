<template>
  <div class="tui-live-kit">
    <live-header :user="{name: userName, userId: userId, avatarUrl: avatarUrl as string}"  @logout="onLogout"/>
    <div class="tui-live-layout">
      <div class="tui-layout-left">
        <div class="tui-live-config-container">
          <live-config @edit-media-source="onEditMediaSource" />
        </div>
        <!-- To do: 暂时屏蔽 
          <div class="tui-live-tool-container">
          <live-tool />
        </div> -->
      </div>
      <div class="tui-layout-middle">
        <div class="tui-live-preview-container">
          <live-preview  @edit-media-source="onEditMediaSource" />
        </div>
        <div class="tui-live-controller-container">
          <live-controller @on-start-living="startLiving" @on-stop-living="stopLiving"/>
        </div>
      </div>
      <div class="tui-layout-right">
        <div class="tui-live-member-container">
          <live-member />
        </div>
        <div class="tui-live-message-container">
          <live-message/>
        </div>
      </div>
      <live-image-source ref="imageSourceRef" v-if="mediaSourceInEdit" v-show="false" :data="mediaSourceInEdit"></live-image-source>
    </div>
  </div>
</template>

<script setup lang="ts">
import "./assets/global.scss";
import { ref, Ref, defineExpose, defineEmits, toRaw, onMounted, onUnmounted, onBeforeUnmount, nextTick } from 'vue';
import { storeToRefs } from "pinia";
import { TRTCStatistics, TRTCVideoStreamType } from 'trtc-electron-sdk';
import { TUIDeviceType, TUIDeviceState } from '@tencentcloud/tuiroom-engine-electron/plugins/device-manager-plugin';
import TUIRoomEngine, { TUIRoomEvents, TUIRoomType, TUIRoomInfo, TUIRequest, TUIRequestAction, TUISeatMode, TUISeatInfo } from '@tencentcloud/tuiroom-engine-electron';
import { TUIMediaSourceType } from '@tencentcloud/tuiroom-engine-electron/plugins/media-mixing-plugin';
import LiveHeader from "./components/LiveHeader/Index.vue";
import LiveConfig from "./components/LiveConfig/Index.vue";
// import LiveTool from "./components/LiveTool/Index.vue";
import LivePreview from "./components/LivePreview/Index.vue";
import LiveController from "./components/LiveController/Index.vue";
import LiveMember from "./components/LiveMember/Index.vue";
import LiveMessage from "./components/LiveMessage/Index.vue";
import LiveImageSource from './components/LiveSource/LiveImageSource.vue';
import { initCommunicationChannels, messageChannels } from "./communication";
import { useBasicStore } from "./store/basic";
import { useRoomStore } from "./store/room";
import { useChatStore } from './store/chat'
import { TUIMediaSourceViewModel, useMediaSourcesStore } from './store/mediaSources';
import useDeviceManagerPlugin from './utils/useDeviceManagerPlugin';
import useMedisMixingPlugin from './utils/useMediaMixingPlugin';
import videoEffectManager from "./utils/useVideoEffectPlugin";
import useAudioEffectManagerPlugin, { TUIVoiceReverbType, TUIVoiceChangerType } from "./utils/useAudioEffectManagerPlugin";
import useGetRoomEngine from "./utils/useRoomEngine";
import trtcCloud from './utils/trtcCloud';
import useMessageHook from "./components/LiveMessage/useMessageHook";
import useErrorHandler from "./hooks/useErrorHandler";
const roomEngine = useGetRoomEngine();

interface UserInfo {
  userId: string,
  userName: string,
  avatarUrl: string,
}

defineExpose({
  init,
});

const emit = defineEmits([
  "on-start-living",
  "on-stop-living",
  "on-logout",
  "on-create-room",
  "on-enter-room",
  "on-destroy-room",
  "on-exit-room",
]);


const mediaMixingPlugin = useMedisMixingPlugin();
const deviceManagerPlugin = useDeviceManagerPlugin();
const audioEffectManager = useAudioEffectManagerPlugin();

const logger = console;
const logPrefix = "[LiveKit]";

const basicStore = useBasicStore();
const roomStore = useRoomStore();
const chatStore = useChatStore();
const mediaSourcesStore = useMediaSourcesStore();
const { userName, userId, avatarUrl } = storeToRefs(basicStore);
const { onError } = useErrorHandler();
const mediaSourceInEdit: Ref<TUIMediaSourceViewModel | null> = ref(null);
const imageSourceRef = ref();

const onEditMediaSource = (mediaSource: TUIMediaSourceViewModel) => {
  logger.log(logPrefix, "onEditMediaSource:", mediaSource);
  let command = '';
  switch(mediaSource.mediaSourceInfo.sourceType) {
  case TUIMediaSourceType.kCamera:
    command = 'camera';
    break;
  case TUIMediaSourceType.kScreen:
    command = 'screen';
    break;
  case TUIMediaSourceType.kImage:
    mediaSourceInEdit.value = toRaw(mediaSource);
    nextTick(() => {
      imageSourceRef.value.triggerFileSelect();
    });
    return;
  default:
    logger.error('onEditMediaSource: sourceType not supported', mediaSource.mediaSourceInfo.sourceType);
  }
  if (!command) {
    return;
  }
  window.ipcRenderer.send('open-child', {
    command,
    data: JSON.parse(JSON.stringify(mediaSource)),
  });
}

onMounted(() => {
  // 初始化主窗口和子窗口直接通信的 MessageChannel
  initCommunicationChannels({
    mediaSourcesStore,
    roomStore,
  });

  // // 初始化美颜管理器
  // videoEffectManager.init(); // To do: 在添加摄像头之前初始化，会导致摄像头打不开，应该是底层 C++ 的限制或者 bug

  window.addEventListener('beforeunload', onBefireUnload);
});

onUnmounted(() => {
  // 清理美颜管理器
  videoEffectManager.clear();
  window.removeEventListener('beforeunload', onBefireUnload);
});

function onLogout() {
  mediaMixingPlugin.setDisplayParams(new Uint8Array(8) ,{ left: 0, right: 0, top: 0, bottom: 0 });
  emit("on-logout");
}

function onBefireUnload() {
  logger.log(`${logPrefix}onBefireUnload`);
  if (basicStore.isLiving) {
    stopLiving();
  }
}

// ***************** 暴露到组件外方法 start *****************
function _generateRoomId() {
  return Math.floor(Math.random() * 1000 * 1000);
}

async function startLiving(){
  logger.log(`${logPrefix}startLiving`);
  try {
    if (basicStore.userId) {
      const roomId = _generateRoomId().toString();
      basicStore.setRoomId(roomId);
      await createRoom({
        roomId,
        roomName: basicStore.roomName,
        roomMode: "SpeakAfterTakingSeat",
      });
      basicStore.setIsLiving(true);
      useMessageHook(); // To do: 先改为在 LiveKit/index.vue 中调用，避免无法接收消息，正常应该在 MessageList.vue 中调用
      emit('on-start-living');

      // publish CDM
      trtcCloud.startPublishing(roomId, TRTCVideoStreamType.TRTCVideoStreamTypeBig);
      logger.log(`${logPrefix}CDN URL: https://3891.liveplay.myqcloud.com/live/${roomId}.flv`);

      // To do: 暂时屏蔽
      // // To do: 音效插件测试，待删除，改为 UI 操作控制 - start
      // await roomEngine.instance?.openLocalMicrophone();
      // await roomEngine.instance?.unmuteLocalAudio();
      // audioEffectManager.setVoiceReverbType(TUIVoiceReverbType.TUILiveVoiceReverbType_3);
      // audioEffectManager.setVoiceChangerType(TUIVoiceChangerType.TUIVoiceChangerType_1);

      // audioEffectManager.setMusicObserver({
      //   onStart: (id: number, errCode: number)=>{
      //     logger.log(`music start play:`, id, errCode);
      //   },
      //   onPlayProgress: (id: number, curPtsMS: number, durationMS: number) => {
      //     logger.log(`music play progress:`, id, curPtsMS, durationMS);
      //   },
      //   onComplete: (id: number, errCode: number) => {
      //     logger.log(`music complete:`, id, errCode);
      //   },
      // });
      // audioEffectManager.startPlayMusic({
      //   id: 1,
      //   path: 'https://web.sdk.qcloud.com/trtc/electron/download/resources/media/bgm/PositiveHappyAdvertising.mp3',
      //   publish: true,
      //   loopCount: 2,
      //   isShortFile: false,
      //   startTimeMS: 0,
      //   endTimeMS: 0,
      // })
      // // To do: 音效插件测试，待删除，改为 UI 操作控制 - end
    } else {
      const errMsg = `${logPrefix}startLiving failed due to no valid userId`;
      logger.error(errMsg);
      alert(errMsg); // To do: 待修改为 Message 组件
    } 
  } catch (error) {
    logger.error(`${logPrefix}startLiving error:`, error);
  }
}

async function stopLiving(){
  logger.log(`${logPrefix}stopLiving`);
  try {
    await dismissRoom();
    basicStore.setIsLiving(false);
    basicStore.setRoomId('');
    roomStore.reset();
    chatStore.reset();
    basicStore.reset();
    messageChannels.childWindowPort?.postMessage({
      key: "reset",
      data: {}
    });
    emit('on-stop-living');

    trtcCloud.stopPublishing();
  } catch (error) {
    logger.error(`${logPrefix}stopLiving error:`, error);
  }
}

type DeviceParams = {
	isOpenCamera: boolean;
	isOpenMicrophone: boolean;
	defaultCameraId: string;
	defaultMicrophoneId: string;
	defaultSpeakerId: string;
}

type RoomInitData = {
  sdkAppId: number;
  userId: string;
  userSig: string;
  userName: string;
  avatarUrl: string;
}

async function init(options: RoomInitData) {
  logger.log(`${logPrefix}init:`, options);
  basicStore.setBasicInfo(options);
  roomStore.setLocalUser(options);
  const { sdkAppId, userId, userSig, userName, avatarUrl } = options;
  await TUIRoomEngine.login({ sdkAppId, userId, userSig });
  await TUIRoomEngine.setSelfInfo({ userName, avatarUrl });
}

async function doEnterRoom(roomId: string) {
  trtcCloud.setDefaultStreamRecvMode(true, false);
  // trtcCloud.enableSmallVideoStream(true, smallParam);// To do
  const roomInfo = await roomEngine.instance?.enterRoom({ roomId, roomType: TUIRoomType.kLive }) as TUIRoomInfo;
  roomEngine.instance?.muteLocalAudio();
  if (!roomInfo.isSeatEnabled) {
    roomEngine.instance?.openLocalMicrophone();
    basicStore.setIsOpenMic(true);
  }
  return roomInfo;
}

async function createRoom(options: {
  roomId: string;
  roomName: string;
  roomMode: 'SpeakAfterTakingSeat';
  deviceParam?: DeviceParams;
}) {
  const { roomId, roomName, roomMode, deviceParam } = options;
  try {
    if (!roomEngine.instance) {
      return;
    }
    basicStore.setRoomId(roomId);
    logger.debug(`${logPrefix}createRoom:`, roomId, roomMode, deviceParam);
    const roomParams = {
      roomId,
      roomName,
      roomType: TUIRoomType.kLive,
      isSeatEnabled: true,
      seatMode: TUISeatMode.kApplyToTake,
      maxSeatCount: 9,
    }
    await roomEngine.instance?.createRoom(roomParams);
    emit("on-create-room", { code: 0, message: 'create room success', });

    await enterRoom(options);    
  } catch (error) {
    logger.error(`${logPrefix}createRoom error:`, error);
    basicStore.reset();
    throw error;
  }
}

async function enterRoom(options: {roomId: string, roomParam?: DeviceParams }) {
  const { roomId, roomParam } = options;
  try {
    if (!roomEngine.instance) {
      return;
    }
    basicStore.setRoomId(roomId);
    logger.debug(`${logPrefix}enterRoom:`, roomId, roomParam);
    const roomInfo = await doEnterRoom(roomId);
    roomStore.setRoomInfo(roomInfo);

    const loginUserInfo = await TUIRoomEngine.getSelfInfo();
    roomStore.setLocalUser(loginUserInfo);
    // await this.getUserList();
    if (roomInfo.isSeatEnabled) {
      // await this.getSeatList();
      if (roomStore.isMaster) {
        // 申请发言模式房主上麦
        await roomEngine.instance?.takeSeat({ seatIndex: -1, timeout: 0 });
        roomEngine.instance?.unmuteLocalAudio();
        roomEngine.instance?.openLocalMicrophone();
        basicStore.setIsOpenMic(true);
      }
    }
    /**
     * setRoomParam must come after setRoomInfo,because roomInfo contains information
     * about whether or not to turn on the no-mac ban.
     **/
    // roomStore.setRoomParam(roomParam);
    emit("on-enter-room", { code: 0, message: 'enter room success', });
  } catch (error) {
    logger.error(`${logPrefix}enterRoom error:`, error);
    basicStore.reset();
    throw error;
  }
}

async function dismissRoom() {
  try {
    logger.log(`${logPrefix}dismissRoom: enter`);
    await closeMediaBeforeLeave();
    await roomEngine.instance?.destroyRoom();
    emit("on-destroy-room");
  } catch (error) {
    logger.error(`${logPrefix}dismissRoom error:`, error);
  }
}

async function leaveRoom() {
  try {
    await closeMediaBeforeLeave();
    const response = await roomEngine.instance?.exitRoom();
    emit("on-exit-room");
    logger.log(`${logPrefix}leaveRoom:`, response);
  } catch (error) {
    logger.error(`${logPrefix}leaveRoom error:`, error);
  }
}

async function closeMediaBeforeLeave() {
  // To do: to be implemented
  // if (localUser.value.hasAudioStream) {
  //   await roomEngine.instance?.closeLocalMicrophone();
  // }
  // if (localUser.value.hasVideoStream) {
  //   // await roomEngine.instance?.closeLocalCamera();
  //   await mediaMixingPlugin.stopPublish();
  // }
}
// ***************** 暴露到组件外方法 end *****************

// ***************** 处理 Room Engine 事件监听 start *****************
function onRemoteUserEnterRoom(eventInfo: { userInfo: UserInfo }) {
  roomStore.addRemoteUser(eventInfo.userInfo);
}

function onRemoteUserLeaveRoom(eventInfo: { userInfo: UserInfo }) {
  roomStore.removeRemoteUser(eventInfo.userInfo.userId);
}

// 收到来自用户的上麦申请
function onRequestReceived(eventInfo: { request: TUIRequest }) {
  const { requestAction, requestId, userId, timestamp } = eventInfo.request;
  if (requestAction === TUIRequestAction.kRequestToTakeSeat) {
    userId && roomStore.addApplyToAnchorUser({ userId, requestId, timestamp });
  }
}

// 远端用户取消上麦申请
function onRequestCancelled(eventInfo: { requestId: string; userId: string; request: TUIRequest }) {
  logger.log(`${logPrefix}onRequestCancelled:`, eventInfo);
  const { userId, request } = eventInfo;
  if (request.requestAction === TUIRequestAction.kRequestToTakeSeat) {
    roomStore.removeApplyToAnchorUser(userId);
  }
}

// 麦位变化
function onSeatListChanged(eventInfo: { seatList: TUISeatInfo[], seatedList: TUISeatInfo[], leftList: TUISeatInfo[] }) {
  logger.log(`${logPrefix}onSeatListChanged:`, eventInfo);
  const { seatedList, leftList } = eventInfo;
  roomStore.updateOnSeatList(seatedList, leftList);
}

TUIRoomEngine.once('ready', () => {
  roomEngine.instance?.on(TUIRoomEvents.onError, onError);
  roomEngine.instance?.on(TUIRoomEvents.onRemoteUserEnterRoom, onRemoteUserEnterRoom);
  roomEngine.instance?.on(TUIRoomEvents.onRemoteUserLeaveRoom, onRemoteUserLeaveRoom);
  roomEngine.instance?.on(TUIRoomEvents.onRequestReceived, onRequestReceived);
  roomEngine.instance?.on(TUIRoomEvents.onRequestCancelled, onRequestCancelled);
  roomEngine.instance?.on(TUIRoomEvents.onSeatListChanged, onSeatListChanged);
});

onUnmounted(() => {
  roomEngine.instance?.off(TUIRoomEvents.onError, onError);
  roomEngine.instance?.off(TUIRoomEvents.onRemoteUserEnterRoom, onRemoteUserEnterRoom);
  roomEngine.instance?.off(TUIRoomEvents.onRemoteUserLeaveRoom, onRemoteUserLeaveRoom);
  roomEngine.instance?.off(TUIRoomEvents.onRequestReceived, onRequestReceived);
  roomEngine.instance?.off(TUIRoomEvents.onRequestCancelled, onRequestCancelled);
  roomEngine.instance?.off(TUIRoomEvents.onSeatListChanged, onSeatListChanged);
});
// ***************** 处理 Room Engine 事件监听 end *****************

// ***************** 设备事件 start *****************
function onDeviceChanged(deviceId: string, type: TUIDeviceType, state: TUIDeviceState): void{
  logger.debug(`${logPrefix}onDeviceChanged: deviceId:${deviceId}, type:${type}, state:${state}`);
  messageChannels.childWindowPort?.postMessage({
    key: "on-device-changed",
    data: {
      deviceId,
      type,
      state,
    }
  });
}

function onTestMicVolume(volume: number) {
  messageChannels.childWindowPort?.postMessage({
    key: "update-audio-volume",
    data: volume
  });
}

function onTestSpeakerVolume(volume: number) {
  messageChannels.childWindowPort?.postMessage({
    key: "update-speaker-volume",
    data: volume
  });
}

function onStatistics(statis: TRTCStatistics) {
  basicStore.setStatistics(statis);
}

onMounted(() => {
  deviceManagerPlugin.on("onDeviceChanged", onDeviceChanged);
  trtcCloud.on("onTestMicVolume", onTestMicVolume);
  trtcCloud.on("onTestSpeakerVolume", onTestSpeakerVolume);
  trtcCloud.on("onStatistics", onStatistics);
});

onBeforeUnmount(() => {
  deviceManagerPlugin.off("onDeviceChanged", onDeviceChanged);
  trtcCloud.off("onTestMicVolume", onTestMicVolume);
  trtcCloud.off("onTestSpeakerVolume", onTestSpeakerVolume);
  trtcCloud.off("onStatistics", onStatistics);
});
// ***************** 设备事件 end *****************
</script>

<style scoped lang="scss">
@import "./assets/variable.scss";
@import "./assets/global.scss";

.tui-live-kit {
  width: 100%;
  height: 100%;
  padding: 0 0.5rem 0.5rem 0.5rem;
  background-color: $color-background-primary;
  color: $color-font-default;
  font-size: 0.75rem;

  .tui-live-layout {
    width: 100%;
    height: calc(100% - 2.75rem);
    border-radius: 0.5rem;

    display: flex;
    flex-direction: row;
  }

  .tui-layout-left, .tui-layout-right {
    flex: 0 0 18rem;
    display: flex;
    flex-direction: column;
  }

  .tui-layout-middle {
    flex: 1 1 auto;
    display: flex;
    flex-direction: column;
    padding: 0 0.5rem;
  }

  .tui-live-config-container {
    height: 100%;
    flex: 1 1 auto;
    border-radius: 0.5rem 0 0 0;
    background-color: $color-background-secondary;
  }

  .tui-live-tool-container {
    margin-top: 0.5rem;
    flex: 0 0 12rem;
    background-color: $color-background-secondary;
    border-radius: 0 0 0 0.5rem;
    padding: 0 1rem;
  }

  .tui-live-preview-container {
    flex: 1 1 auto;
    background-color: $color-background-secondary;
  }

  .tui-live-controller-container {
    flex: 0 0 4rem;
    height: 4rem;
    background-color: $color-background-secondary;
  }

  .tui-live-member-container {
    flex: 1 1 40%;
    height: 40%;
    background-color: $color-background-secondary;
    border-radius: 0 0.5rem 0 0;
  }

  .tui-live-message-container {
    flex: 1 1 auto;
    margin-top: 0.5rem;
    height: calc(60% - 0.5rem);
    background-color: $color-background-secondary;
    border-radius: 0 0 0.5rem 0;
  }
}
</style>