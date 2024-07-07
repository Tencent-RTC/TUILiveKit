import { TRTCVideoBufferType, TRTCVideoPixelFormat } from 'trtc-electron-sdk';
import TUIVideoEffectPluginManager, { TUIVideoEffectPlugin } from '@tencentcloud/tuiroom-engine-electron/plugins/video-effect-plugin';
import { TRTCXmagicFactory, XmagicLicense, TRTCXmagicEffectProperty } from "./beauty";

const logger = console;

class TUIVideoEffectManager {
  private logPrefix = '[TUIVideoEffectManager]';
  private inited: boolean;
  private libPath: string;
  private initParam: Record<string, string>;
  private beautyPluginMap: Map<string, TUIVideoEffectPlugin>;

  static getInstance() {
    return new TUIVideoEffectManager();
  }

  constructor() {
    this.inited = false;
    this.libPath = '';
    this.initParam = {};
    this.beautyPluginMap = new Map();

    // this.init(); // To do: 不能在添加摄像头之前初始化，否则会导致摄像头打不开，可能是底层 C++ 的限制或者 bug
  }

  async init() {
    if (!this.inited) {
      TUIVideoEffectPluginManager.setVideoPluginFormat(
        TRTCVideoBufferType.TRTCVideoBufferType_Buffer,
        TRTCVideoPixelFormat.TRTCVideoPixelFormat_I420
      );
      TUIVideoEffectPluginManager.setCallback((pluginId: string, errorCode: number, errorMessage: string) => {
        logger.log(`plugin event, pluginId : ${pluginId}, errorCode : ${errorCode}, errorMessage : ${errorMessage}`);
      });
      this.libPath = await TRTCXmagicFactory.getEffectPluginLibPath();
      this.initParam = await TRTCXmagicFactory.buildEffectInitParam(XmagicLicense);
      this.inited = true;
      logger.log(`${this.logPrefix}init success.`, this.libPath, this.initParam?.licenseURL);
    }
  }

  async startEffect(cameraId: string, beautyConfig: { beautyProperties: TRTCXmagicEffectProperty[] }) {
    logger.debug(`${this.logPrefix}startEffect ${cameraId}`, beautyConfig);
    if (!this.inited) {
      await this.init();
    }

    let plugin = this.beautyPluginMap.get(cameraId) || null;
    if (!plugin) {
      plugin = this.addPlugin(cameraId);
      if (!plugin) {
        logger.error(`${this.logPrefix}startEffect failed. Cannot create effect plugin.`);
        return;
      }
    }
    setTimeout(()=> {
      if (plugin) {
        plugin.setParameter(JSON.stringify({
          beautySetting: beautyConfig.beautyProperties
        }));
      }
    }, 3000);
  }

  stopEffect(cameraId: string) {
    logger.debug(`${this.logPrefix}stopEffect ${cameraId}`);
    const plugin = this.beautyPluginMap.get(cameraId);
    if (plugin) {
      TUIVideoEffectPluginManager.removeVideoPlugin(plugin.deviceId, plugin.id);
      this.beautyPluginMap.delete(cameraId);
    } else {
      logger.error(`${this.logPrefix}stopEffect failed. No effect plugin for camera:`, cameraId);
    }
  }

  muteEffect(cameraId: string, mute: boolean) {
    logger.debug(`${this.logPrefix}muteEffect ${cameraId}`, mute);
    const plugin = this.beautyPluginMap.get(cameraId);
    if (plugin) {
      plugin.enable(!mute);
    } else {
      logger.error(`${this.logPrefix}muteEffect failed. No effect plugin for camera:`, cameraId);
    }
  }

  updateEffect(cameraId: string, beautyConfig: { beautyProperties: TRTCXmagicEffectProperty[] }) {
    logger.debug(`${this.logPrefix}updateEffect ${cameraId}`, beautyConfig);
    const plugin = this.beautyPluginMap.get(cameraId);
    if (plugin) {
      plugin.setParameter(JSON.stringify({
        beautySetting: beautyConfig.beautyProperties
      }));
    } else {
      logger.error(`${this.logPrefix}updateEffect failed. No effect plugin for camera:`, cameraId);
    }
  }

  clear() {
    logger.debug(`${this.logPrefix}clear`);
    this.beautyPluginMap.forEach((plugin) => {
      TUIVideoEffectPluginManager.removeVideoPlugin(plugin.deviceId, plugin.id);
    });
    this.beautyPluginMap.clear();
  }

  private addPlugin(cameraId: string): TUIVideoEffectPlugin | null {
    const pluginId = `${cameraId}-${new Date().getTime()}`; // ID 可以随意设置，只要唯一、不重复就行
    const plugin: TUIVideoEffectPlugin | null = TUIVideoEffectPluginManager.addVideoPlugin({
      pluginId: pluginId,
      deviceId: cameraId,
      path: this.libPath,
    });
    if (plugin) {
      this.beautyPluginMap.set(cameraId, plugin);
      plugin.enable(true);
      plugin.setParameter(JSON.stringify(this.initParam)); 
      logger.debug(`${this.logPrefix}addPlugin success for camera: ${cameraId}`);
      return plugin;
    } else {
      logger.error(`${this.logPrefix}addPlugin failed for camera: ${cameraId}`);
      return null;
    }
  }

  private getPlugin(cameraId: string): TUIVideoEffectPlugin | null {
    return this.beautyPluginMap.get(cameraId) || null;
  }
}

const videoEffectManager = TUIVideoEffectManager.getInstance();

export default videoEffectManager;
