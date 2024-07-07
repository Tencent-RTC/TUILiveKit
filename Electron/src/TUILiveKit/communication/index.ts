import { TRTCVideoRotation } from 'trtc-electron-sdk';
import useDeviceManagerPlugin from "../utils/useDeviceManagerPlugin";
import { TUIMediaSourceType } from "../utils/useMediaMixingPlugin";
import { TUIMediaSourceViewModel } from '../store/mediaSources';
import useGetRoomEngine from "../utils/useRoomEngine";
import { TRTCXmagicFactory, XmagicLicense, } from "../utils/beauty";
import TUIMessageBox from '../common/base/MessageBox';
import { useI18n } from '../locales/index';
import trtcCloud from '../utils/trtcCloud';

const { t } = useI18n();

const logger = console;
const logPrefix = "[MainWindow Message Handler]";

const roomEngine = useGetRoomEngine()
const deviceManagerPlugin = useDeviceManagerPlugin();
let mediaSourcesStore: any = null;
let roomStore: any = null;

export const messageChannels: {
  childWindowPort: MessagePort | null;
  contextWindowPort: MessagePort | null;
} = {
  childWindowPort: null,
  contextWindowPort: null,
};

(window as any)._messageChannels = messageChannels; // To do: 待删除，方便调试

export async function addMediaSource(data: Record<string, any>) {
  const mediaSource: TUIMediaSourceViewModel = {
    sourceName: data.name,
    aliasName: data.name,
    left: 0,
    top: 0,
    muted: false,
    mediaSourceInfo: {
      sourceType: data.type,
      sourceId: data.id,
      zOrder: 1,
      rect: {
        left: 0,
        top: 0,
        right: data.width || 640, // To do：bugfix 偶尔会拿不到摄像头支持的采集分辨率
        bottom: data.height || 320, // To do：bugfix 偶尔会拿不到摄像头支持的采集分辨率
      }
    },
  }
  if (data.type === TUIMediaSourceType.kCamera) {
    mediaSource.resolution = {
      width: data.width,
      height: data.height,
    }
    mediaSource.mediaSourceInfo.mirrorType = data.mirrorType;

    if (data.beautyConfig) {
      mediaSource.beautyConfig = data.beautyConfig;
    }
  }else if(data.type === TUIMediaSourceType.kScreen){
    mediaSource.screenType = data.screenType;
  }

  if (mediaSource) {
    logger.log(`${logPrefix}addMediaSource:`, mediaSource);
    try {
      await mediaSourcesStore.addMediaSource(mediaSource);
      await mediaSourcesStore.selectMediaSource(mediaSource);
    } catch (error) {
      TUIMessageBox({
        title: t('Note'),
        message: (error as any).message,
        confirmButtonText: t('Sure'),
      });
    }
  }
}


function checkRectAndResolution(
  newSize: { width: number, height: number },
  rect: { left: number, top: number, right: number, bottom: number },
  rotation: TRTCVideoRotation
) {
  let width = rect.right - rect.left;
  let height = rect.bottom - rect.top;
  if (rotation === TRTCVideoRotation.TRTCVideoRotation90 || rotation === TRTCVideoRotation.TRTCVideoRotation270) {
    const temp = width;
    width = height;
    height = temp;
  }
  const shrinkRate = width / newSize.width > height / newSize.height ? height / newSize.height : width / newSize.width;

  if (rotation === TRTCVideoRotation.TRTCVideoRotation90 || rotation === TRTCVideoRotation.TRTCVideoRotation270) {
    rect.right = rect.left + Math.round(newSize.height * shrinkRate);
    rect.bottom = rect.top + Math.round(newSize.width * shrinkRate);
  } else {
    rect.right = rect.left + Math.round(newSize.width * shrinkRate);
    rect.bottom = rect.top + Math.round(newSize.height * shrinkRate);
  }
  return rect;
}
 

async function _updateScreenImageMediaSource(data: Record<string, any>) {
  logger.log(`${logPrefix}updateMediaSource predata:`, JSON.stringify(data.predata));
  if (data.id !== data.predata?.mediaSourceInfo.sourceId) {
    const newMediaSource: TUIMediaSourceViewModel = {
      sourceName: data.name,
      aliasName: data.predata.aliasName,
      left: data.predata?.left,
      top: data.predata?.top,
      muted: false,
      mediaSourceInfo: {
        sourceType: data.type,
        sourceId: data.id,
        zOrder: data.predata?.mediaSourceInfo.zOrder,
        rect: checkRectAndResolution({ width: data.width, height: data.height }, data.predata?.mediaSourceInfo.rect, data.predata?.mediaSourceInfo.rotation),
        rotation: data.predata?.mediaSourceInfo.rotation,
      }
    };

    if(data.type === TUIMediaSourceType.kScreen){
      newMediaSource.screenType = data.screenType;
    }

    if (data.predata && newMediaSource) {
      logger.log(`${logPrefix}updateMediaSource newdata:`, JSON.stringify(newMediaSource));
      try {
        await mediaSourcesStore.replaceMediaSource(data.predata, newMediaSource);
        await mediaSourcesStore.selectMediaSource(newMediaSource);
      } catch (error) {
        TUIMessageBox({
          title: t('Note'),
          message: (error as any).message,
          confirmButtonText: t('Sure'),
        });
      }
    } else {
      logger.error(`${logPrefix}updateMediaSource invalid data:`, data.predata, newMediaSource);
    }
  } else {
    logger.warn(`${logPrefix}updateMediaSource with data not changed:`, data);
  }
}

async function _updateCameraMediaSource(data: Record<string, any>) {
  logger.log(`${logPrefix}updateMediaSource predata:`, JSON.stringify(data.predata));
  const newMediaSource: TUIMediaSourceViewModel = {
    sourceName: data.name,
    aliasName: data.predata.aliasName,
    left: data.predata?.left,
    top: data.predata?.top,
    muted: false,
    resolution: {
      width: data.width,
      height: data.height
    },
    mediaSourceInfo: {
      sourceType: data.type,
      sourceId: data.id,
      zOrder: data.predata?.mediaSourceInfo.zOrder,
      rect: checkRectAndResolution({ width: data.width, height: data.height }, data.predata?.mediaSourceInfo.rect, data.predata?.mediaSourceInfo.rotation),
      mirrorType: data.mirrorType,
      rotation: data.predata?.mediaSourceInfo.rotation,
    },
    beautyConfig: data.beautyConfig
  };
  logger.log(`${logPrefix}updateMediaSource newdata:`, JSON.stringify(newMediaSource));


  try {
    if (data.id === data.predata?.mediaSourceInfo.sourceId) {
      await mediaSourcesStore.updateMediaSource(newMediaSource);
    } else {
      await mediaSourcesStore.replaceMediaSource(data.predata, newMediaSource);
    }
    await mediaSourcesStore.selectMediaSource(newMediaSource);
  } catch (error) {
    TUIMessageBox({
      title: t('Note'),
      message: (error as any).message,
      confirmButtonText: t('Sure'),
    });
  }
}

export async function updateMediaSource(data: Record<string, any>) {
  switch (data.type) {
  case TUIMediaSourceType.kScreen:
  case TUIMediaSourceType.kImage:
    await _updateScreenImageMediaSource(data);
    break;
  case TUIMediaSourceType.kCamera:
    await _updateCameraMediaSource(data);
    break;
  default:
    logger.warn(`${logPrefix}updateMediaSource un-supported media type:`, data);
    break;
  }
}

async function handleUserApply(data: Record<string, any>) {
  const { agree } = data;
  const user = JSON.parse(data.user);
  if (user.userId) {
    roomStore.handleApplyToAnchorUser(user.userId, agree);
  }
}

async function handleKickSeat(data: Record<string, any>) {
  const { userId } = data;
  roomStore.kickUserOffSeat(userId);
}

async function handleKickOut(data: Record<string, any>) {
  const { userId } = data;
  roomStore.kickUserOutOfRoom(userId);
}

async function handleChildWindowMessage(event: MessageEvent<any>) {
  console.log(`${logPrefix}handleChildWindowMessage:`, event.data);
  const { key, data } = event.data;

  switch (key) {
  case "setCurrentDevice":
    deviceManagerPlugin.setCurrentDevice(data.deviceType, data.deviceId);
    break;
  case "startCameraDeviceTest":
    deviceManagerPlugin.startCameraDeviceTest(data.windowID, data.rect);
    if (data.log) {
      trtcCloud?.log(data.log);
    }
    break;
  case "setCameraTestRenderMirror":
    deviceManagerPlugin.setCameraTestRenderMirror(data.mirror);
    break;
  case "setCameraTestResolution":
    deviceManagerPlugin.setCameraTestResolution(data.width, data.height);
    break;
  case "setCameraTestDeviceId":
    deviceManagerPlugin.setCameraTestDeviceId(data.cameraId);
    break;
  case "stopCameraDeviceTest":
    deviceManagerPlugin.stopCameraDeviceTest();
    break;
  case "setCameraTestVideoPluginPath":
    if(data){
      const beautyLibPath = await TRTCXmagicFactory.getEffectPluginLibPath();
      const beautyInitParam = await TRTCXmagicFactory.buildEffectInitParam(XmagicLicense);
      deviceManagerPlugin.setCameraTestVideoPluginPath(beautyLibPath);
      deviceManagerPlugin.setCameraTestVideoPluginParameter(JSON.stringify(beautyInitParam));
    }else{
      deviceManagerPlugin.setCameraTestVideoPluginPath('');
    }
    break;
  case "setCameraTestVideoPluginParameter":
    deviceManagerPlugin.setCameraTestVideoPluginParameter(JSON.stringify({
      beautySetting: Array.isArray(data) ? data : [data],
    }));
    break;
  case "addMediaSource":
    addMediaSource(data);
    break;
  case "updateMediaSource":
    updateMediaSource(data);
    break;
  case "handleUserApply":
    handleUserApply(data);
    break;
  case "cancelWheatPosition":
    handleKickSeat(data);
    break;
  case "kickOut":
    handleKickOut(data);
    break;
  case "startTestSpeaker":
    deviceManagerPlugin.startSpeakerDeviceTest(data);
    break;
  case "stopTestSpeaker":
    deviceManagerPlugin.stopSpeakerDeviceTest();
    break;
  case "startTestMic":
    deviceManagerPlugin.startMicDeviceTest(data.interval, data.playback);
    break;
  case "stopTestMic":
    deviceManagerPlugin.stopMicDeviceTest();
    break;
  default:
    console.warn(`${logPrefix}handleChildWindowMessage: unsupported key: ${key}`);
    break;
  }
}

export function initCommunicationChannels(data: Record<string, any>) {
  mediaSourcesStore = data.mediaSourcesStore;
  roomStore = data.roomStore;
  const childChannel = new MessageChannel();

  const childChannelServer = childChannel.port1;
  const childChannelClient = childChannel.port2;

  childChannelServer.onmessage = handleChildWindowMessage;
  childChannelServer.onmessageerror = (event) => {
    console.log('onMessageFromChildWindowError', event.data)
  };
  childChannelServer.start();

  console.log('port-to-child')
  window.ipcRenderer.postMessage('port-to-child', null, [childChannelClient]);

  messageChannels.childWindowPort = childChannelServer;


  const contextChannel = new MessageChannel();

  const contextChannelServer = contextChannel.port1;
  const contextChannelClient = contextChannel.port2;

  contextChannelServer.onmessage = (event) => {
    console.log('onMessageFromContextWindow', event.data)
  }
  contextChannelServer.onmessageerror = (event) => {
    console.log('onMessageFromContextWindowError', event.data)
  }
  contextChannelServer.start();

  console.log('port-to-context')
  window.ipcRenderer.postMessage('port-to-context', null, [contextChannelClient]);

  messageChannels.contextWindowPort = contextChannelServer;
}
