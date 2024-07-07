<template>
  <div class="child-window">
    <live-camera-source v-if="currentViewName === 'camera'" :data="dataInEdit"></live-camera-source>
    <live-screen-share-source v-if="currentViewName === 'screen'" :data="dataInEdit"></live-screen-share-source>
    <live-image-source v-if="currentViewName === 'image'" :data="dataInEdit"></live-image-source>
    <live-voice-chat v-if="currentViewName === 'voice-chat'"></live-voice-chat>
    <live-setting v-if="currentViewName === 'setting'"></live-setting>
  </div>
</template>

<script setup lang="ts">
import { onMounted, ref, Ref } from 'vue';
import { TRTCScreenCaptureSourceInfo, TRTCScreenCaptureSourceType } from '@tencentcloud/tuiroom-engine-electron';
import { TUIDeviceType, TUIDeviceState } from '@tencentcloud/tuiroom-engine-electron/plugins/device-manager-plugin';
import LiveCameraSource from '../TUILiveKit/components/LiveSource/LiveCameraSource.vue';
import LiveScreenShareSource from '../TUILiveKit/components/LiveSource/LiveScreenShareSource.vue';
import LiveImageSource from '../TUILiveKit/components/LiveSource/LiveImageSource.vue';
import LiveVoiceChat from '../TUILiveKit/components/LiveToolbar/LiveVoiceChat.vue';
import LiveSetting from '../TUILiveKit/components/LiveToolbar/LiveSetting.vue';
import TUIMessageBox from '../TUILiveKit/common/base/MessageBox';
import { useCurrentSourcesStore } from '../TUILiveKit/store/currentSources';
import useDeviceManagerPlugin from '../TUILiveKit/utils/useDeviceManagerPlugin';
import trtcCloud from "../TUILiveKit/utils/trtcCloud";
import { useI18n } from '../TUILiveKit/locales/index';

const { t } = useI18n();

const logger = console;
const logPrefix = "[ChildWindowView]"

const currentSourceStore = useCurrentSourcesStore();
const currentViewName = ref(currentSourceStore.currentViewName);

const deviceManagerPlugin = useDeviceManagerPlugin();

const screenList: Ref<TRTCScreenCaptureSourceInfo[]> = ref([]);
const windowList: Ref<TRTCScreenCaptureSourceInfo[]> = ref([]);

const dataInEdit: Ref<Record<string, any> | undefined> = ref(undefined);

onMounted(() => {
  setTimeout(() => {
    initMainWindowMessageListener();
  }, 3000); // To do: 需要等待一下，主窗口才能把 MessagePort 发送过来。实现不够优先，待优化。
});

function initMainWindowMessageListener() {
  if (window.mainWindowPort) {
    window.mainWindowPort.addEventListener("message", (event) => {
      console.log(`${logPrefix}message from main window:`, event.data, event);
      const { key, data } = event.data;
      switch (key) {
      case "set-apply-list":
        currentSourceStore.setApplyToAnchorList(JSON.parse(data))
        break;
      case "set-anchor-list":
        currentSourceStore.setAnchorList(JSON.parse(data))
        break;
      case "reset":
        currentSourceStore.reset();
        break;
      case "update-audio-volume":
        currentSourceStore.updateAudioVolume(data);
        break;
      case "update-speaker-volume":
        currentSourceStore.updateSpeakerVolume(data);
        break;
      case "on-device-changed":
        onDeviceChanged(data);
        break;
      default:
        logger.warn(`${logPrefix}message from main: not supported message key: ${key}`);
        break;
      }
    });
    window.mainWindowPort.addEventListener("messageerror", (event) => {
      logger.error(`${logPrefix}message from main window error:`, event.data, event);
    });
    window.mainWindowPort.start();
    window.mainWindowPort.postMessage({ key: "notice", data: "child port started"});
  } else {
    logger.error('Cannot add event listener to main window port');
  }
}

function onDeviceChanged(data: { deviceId: string; type: TUIDeviceType; state: TUIDeviceState; }) {
  logger.log(`${logPrefix}onDeviceChanged:`, data);
  let deviceList = null;
  if (data.type === TUIDeviceType.DeviceTypeCamera) {
    deviceList = deviceManagerPlugin.getCameraDevicesList();
    if (deviceList) {
      currentSourceStore.setCameraList(deviceList);
      if (data.state === TUIDeviceState.DeviceStateRemove && data.deviceId === currentSourceStore.currentCameraId) {
        currentSourceStore.setCurrentCameraId(deviceList[0].deviceId);
      }
    }  
  } else if (data.type === TUIDeviceType.DeviceTypeMic) {
    if (data.state === TUIDeviceState.DeviceStateRemove || data.state === TUIDeviceState.DeviceStateAdd) {
      deviceList = deviceManagerPlugin.getMicDevicesList();
      deviceList && currentSourceStore.setMicrophoneList(deviceList);
    } else if (data.state === TUIDeviceState.DeviceStateActive) {
      currentSourceStore.setCurrentMicrophoneId(data.deviceId)
    }
  } else if (data.type === TUIDeviceType.DeviceTypeSpeaker) {
    if (data.state === TUIDeviceState.DeviceStateRemove || data.state === TUIDeviceState.DeviceStateAdd) {
      deviceList = deviceManagerPlugin.getSpeakerDevicesList();
      deviceList && currentSourceStore.setSpeakerList(deviceList);
    } else if (data.state === TUIDeviceState.DeviceStateActive) {
      currentSourceStore.setCurrentSpeakerId(data.deviceId)
    }
  } else {
    logger.warn(`${logPrefix}onDeviceChanged un-supported device type:`, data);
  }
}

function refreshCameraDeviceList() {
  if (deviceManagerPlugin) {
    const cameraList = deviceManagerPlugin.getCameraDevicesList();
    logger.debug(`${logPrefix}camera device list:`, cameraList);
    if (cameraList && cameraList.length > 0) {
      currentSourceStore.setCameraList(cameraList);
    } else {
      currentSourceStore.setCameraList([]);
      TUIMessageBox({
        title: t('Note'),
        message: t('No camera'),
        confirmButtonText: t('Sure'),
      });
    }
  }
}

function refreshMicrophoneDeviceList() {
  if (deviceManagerPlugin) {
    const microphoneList = deviceManagerPlugin.getMicDevicesList();
    logger.debug(`${logPrefix}microphone device list:`, microphoneList);

    if (microphoneList && microphoneList.length > 0) {
      currentSourceStore.setMicrophoneList(microphoneList);
    } else {
      currentSourceStore.setMicrophoneList([]);
      TUIMessageBox({
        title: t('Note'),
        message: t('No microphone'),
        confirmButtonText: t('Sure'),
      });
    }
  }
}

function refreshSpeakerDeviceList() {
  if (deviceManagerPlugin) {
    const speakerList = deviceManagerPlugin.getSpeakerDevicesList();
    logger.debug(`${logPrefix}speaker device list:`, speakerList);

    if (speakerList && speakerList.length > 0) {
      currentSourceStore.setSpeakerList(speakerList);
    } else {
      currentSourceStore.setSpeakerList([]);
      TUIMessageBox({
        title: t('Note'),
        message: t('No speaker'),
        confirmButtonText: t('Sure'),
      });
    }
  }
}

function refreshMediaDeviceList() {
  if (deviceManagerPlugin) {
    refreshCameraDeviceList();
    refreshMicrophoneDeviceList();
    refreshSpeakerDeviceList();
  } else {
    logger.warn(`${logPrefix}init device list failed, deviceManagerPlugin is not existed`);
  }
}

async function refreshScreenList() {
  const thumbWidth = 640;
  const thumbHeight = 360;
  const  iconWidth = 48;
  const iconHeight = 48;

  try {
    const screenCaptureList = await trtcCloud.getScreenCaptureSources(thumbWidth, thumbHeight, iconWidth, iconHeight);
    screenList.value = [];
    windowList.value = [];
    screenCaptureList.forEach((screen: TRTCScreenCaptureSourceInfo) => {
      if (screen.type === TRTCScreenCaptureSourceType.TRTCScreenCaptureSourceTypeWindow) {
        windowList.value.push(screen);
      } else {
        screenList.value.push(screen);
      }
    })
    currentSourceStore.setScreenList(screenList.value);
    currentSourceStore.setWindowList(windowList.value);
  } catch (err) {
    logger.error('获取屏幕/窗口列表失败：',err);
  }
}

const commandHandlers = new Map([
  ['camera', () => {
    refreshMediaDeviceList();
    currentViewName.value = 'camera';
  }],
  ['screen', () => {
    refreshScreenList();
    currentViewName.value = 'screen';
  }],
  ['image', () => {
    currentViewName.value = 'image';
  }],
  ['voice-chat', () => {
    currentViewName.value = 'voice-chat';
  }],
  ['setting', () => {
    refreshMediaDeviceList();
    currentViewName.value = 'setting';
  }],
]);

window.ipcRenderer.on('show', (event, args: Record<string, any>) => {
  logger.log(`${logPrefix}on child show`, args);

  const handler = commandHandlers.get(args?.command);
  if (handler) {
    handler();
  }
  if (args?.data) {
    dataInEdit.value = args.data; // Edit
  } else {
    dataInEdit.value = undefined; // Add
  }
  currentSourceStore.setCurrentViewName(currentViewName.value);
});
</script>
<style scoped lang="scss" >
@import "../TUILiveKit/assets/variable.scss";
@import "../TUILiveKit/assets/global.scss";

.child-window{
  height: 100%;
  background-color: $color-background-primary;
  color: $color-font-default;
}
</style>