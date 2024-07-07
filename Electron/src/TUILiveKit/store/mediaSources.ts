import { defineStore } from 'pinia';
import { TUIMediaSourceType, TUIMediaSource, TUIVideoEncParam } from '@tencentcloud/tuiroom-engine-electron/plugins/media-mixing-plugin';
import { TRTCCameraCaptureMode, TRTCVideoResolution, TRTCVideoResolutionMode, TRTCVideoRotation } from 'trtc-electron-sdk';
import { resolutionMap } from '../utils/trtcCloud';
import useMediaMixingPlugin from "../utils/useMediaMixingPlugin";
import videoEffectManager from "../utils/useVideoEffectPlugin";
import { TRTCXmagicEffectProperty } from '../utils/beauty';
import { useI18n } from '../locales/index';

const { t } = useI18n();


const logger = console;
const mediaMixingPlugin = useMediaMixingPlugin();

export type TUIBeautyConfig = {
  isEnabled: boolean;
  beautyProperties: TRTCXmagicEffectProperty[];
}

export type TUIMediaSourceViewModel = {
  sourceName: string;
  aliasName: string;
  left: number;
  top: number;
  muted: boolean;
  resolution?: {
    width: number;
    height: number;
  };
  mediaSourceInfo: TUIMediaSource;
  beautyConfig?: TUIBeautyConfig;
  screenType?: TUIScreenType;
}

enum TUIScreenType {
  Window = 0,
  Screen = 1
}

type TUISelectedMediaKey = {
  sourceType: TUIMediaSourceType | null;
  sourceId: string;
}

type TUIMediaSourcesState = {
  mixingVideoEncodeParam: TUIVideoEncParam;
  backgroundColor: number;
  mediaList: Array<TUIMediaSourceViewModel>; // 'zOrder' desc order
  selectedMediaKey: TUISelectedMediaKey;
}

function isSameMediaSource(mediaSource1: TUISelectedMediaKey | TUIMediaSource, mediaSource2: TUISelectedMediaKey | TUIMediaSource) {
  return mediaSource1.sourceId === mediaSource2.sourceId && mediaSource1.sourceType === mediaSource2.sourceType;
}

function aliasMediaSource(newMediaSource: TUIMediaSourceViewModel, mediaList: Array<TUIMediaSourceViewModel>) { 
  let newAliasName = '';
  switch (newMediaSource.mediaSourceInfo.sourceType) {
  case TUIMediaSourceType.kCamera:
    newAliasName = t('Camera');
    break;
  case TUIMediaSourceType.kScreen:
    if (newMediaSource.screenType === 0) {
      newAliasName = t('Window');
    } else {
      newAliasName = t('Screen');
    }
    break;
  case TUIMediaSourceType.kImage:
    newAliasName = t('Image');
    break;
  default:
    logger.warn(`[MediaSource]aliasMediaSource unsupported media type:`, newMediaSource);
    return;
  }

  const aliasNameMap = new Map<string, string>();
  mediaList.forEach(mediaItem => {
    aliasNameMap.set(mediaItem.aliasName, '');
  });

  let i = 0;
  while(aliasNameMap.has(newAliasName+(i ? i : ''))) {
    i++;
  }
  newMediaSource.aliasName = newAliasName + (i ? i : '');
}

export const useMediaSourcesStore = defineStore('mediaSources', {
  state: (): TUIMediaSourcesState => ({
    mixingVideoEncodeParam: {
      resMode: TRTCVideoResolutionMode.TRTCVideoResolutionModeLandscape,
      videoResolution: TRTCVideoResolution.TRTCVideoResolution_1920_1080,
      videoFps: 30,
      videoBitrate: 3000,
      minVideoBitrate: 3000,
      enableAdjustRes: false,
    },
    backgroundColor: 0x191919,
    mediaList: [],
    selectedMediaKey: {
      sourceType: null,
      sourceId: '',
    }
  }),
  getters:{
    isHasSources(state) {
      return state.mediaList.length >= 1;
    },
  },
  actions:{
    setMixingVideoEncodeParams(params: TUIVideoEncParam) {
      this.mixingVideoEncodeParam = params;
    },
    updateMixingVideoEncodeParams(params: TUIVideoEncParam) {
      this.mixingVideoEncodeParam = Object.assign({}, this.mixingVideoEncodeParam, params);
    },
    async addMediaSource(mediaSource: TUIMediaSourceViewModel): Promise<void>{
      this.checkMediaExisting(mediaSource);

      mediaSource.mediaSourceInfo.zOrder = this.mediaList.length + 1;
      if (mediaSource.mediaSourceInfo.rect) {
        // 缩小媒体源显示大小， 保证默认不超出视频分辨率大小
        const { width: resWidth, height: resHeight } = resolutionMap[this.mixingVideoEncodeParam.videoResolution];
        const width = mediaSource.mediaSourceInfo.rect.right - mediaSource.mediaSourceInfo.rect.left;
        const height = mediaSource.mediaSourceInfo.rect.bottom - mediaSource.mediaSourceInfo.rect.top;
        if (width > resWidth || height > resHeight) {
          const shrinkRate = width * resHeight > height * resWidth ? resWidth/width : resHeight/height;
          mediaSource.mediaSourceInfo.rect.right = Math.round(width * shrinkRate);
          mediaSource.mediaSourceInfo.rect.bottom = Math.round(height * shrinkRate);
        }
        this.mediaList.unshift(mediaSource);
        aliasMediaSource(mediaSource, this.mediaList);
        await mediaMixingPlugin.addMediaSource(mediaSource.mediaSourceInfo);
        if (mediaSource.mediaSourceInfo.sourceType === TUIMediaSourceType.kCamera) {
          if (mediaSource.resolution) {
            await mediaMixingPlugin.setCameraCaptureParam(mediaSource.mediaSourceInfo.sourceId, {
              mode: TRTCCameraCaptureMode.TRTCCameraCaptureManual,
              width: mediaSource.resolution?.width,
              height: mediaSource.resolution?.height
            });
          }
          
          if (mediaSource.beautyConfig) {
            videoEffectManager.startEffect(mediaSource.mediaSourceInfo.sourceId, mediaSource.beautyConfig);
          }
        }
      } else {
        logger.error("New added media source with no valid rect:", mediaSource);
        throw new Error(t('MediaSource rect invalid or does not exist'));
      }
    },
    async removeMediaSource(mediaSource: TUIMediaSourceViewModel) {
      let indexToRemove = -1;
      const length = this.mediaList.length;
      for(let i = length - 1; i >= 0; i--) {
        const item = this.mediaList[i];
        if (indexToRemove === -1 && isSameMediaSource(item.mediaSourceInfo, mediaSource.mediaSourceInfo)) {
          indexToRemove = i;
          if (mediaSource.mediaSourceInfo.sourceType === TUIMediaSourceType.kCamera) {
            videoEffectManager.stopEffect(mediaSource.mediaSourceInfo.sourceId);
          }
          if (isSameMediaSource(this.mediaList[indexToRemove].mediaSourceInfo, this.selectedMediaKey)) {
            this.selectedMediaKey = {
              sourceId: '',
              sourceType: null,
            }
          }
          if (!this.mediaList[indexToRemove].muted) {
            await mediaMixingPlugin.removeMediaSource(mediaSource.mediaSourceInfo);
          }
          continue;
        }

        if (indexToRemove !== -1) {
          item.mediaSourceInfo.zOrder = (item.mediaSourceInfo.zOrder as number) - 1;
          await mediaMixingPlugin.updateMediaSource(item.mediaSourceInfo);
        }
      }

      if (indexToRemove !== -1) {
        this.mediaList.splice(indexToRemove, 1);
      }
    },
    async updateMediaSource(mediaSource: TUIMediaSourceViewModel) {
      let targetIndex = -1;
      for(let i = 0; i < this.mediaList.length; i++) {
        if (isSameMediaSource(this.mediaList[i].mediaSourceInfo, mediaSource.mediaSourceInfo)) {
          targetIndex = i;
          break;
        }
      }
      if (targetIndex >= 0) {
        const target = this.mediaList[targetIndex];
        if(!target.muted){
          await mediaMixingPlugin.updateMediaSource(mediaSource.mediaSourceInfo);
        } else {
          await mediaMixingPlugin.addMediaSource(mediaSource.mediaSourceInfo);
        }

        if (mediaSource.mediaSourceInfo.sourceType === TUIMediaSourceType.kCamera) {
          if (mediaSource.resolution 
            && (target.resolution?.width !== mediaSource.resolution?.width 
            || target.resolution?.height !== mediaSource.resolution?.height)
          ) {
            await mediaMixingPlugin.setCameraCaptureParam(mediaSource.mediaSourceInfo.sourceId, {
              mode: TRTCCameraCaptureMode.TRTCCameraCaptureManual,
              width: mediaSource.resolution?.width,
              height: mediaSource.resolution?.height
            });
          }

          if (mediaSource.beautyConfig) {
            if(!target.muted){
              videoEffectManager.updateEffect(mediaSource.mediaSourceInfo.sourceId, mediaSource.beautyConfig);
            } else {
              videoEffectManager.startEffect(mediaSource.mediaSourceInfo.sourceId, mediaSource.beautyConfig);
            }
          }
        }
        
        this.mediaList[targetIndex] = {
          ...mediaSource,
          muted: false
        };
      } else {
        logger.warn('[MedisSources]updateMediaSource not found media source:', mediaSource);
      }
    },
    async updateMediaSourceRect(mediaSource: TUIMediaSourceViewModel) {
      let targetIndex = -1;
      for(let i = 0; i < this.mediaList.length; i++) {
        if (isSameMediaSource(this.mediaList[i].mediaSourceInfo, mediaSource.mediaSourceInfo)) {
          targetIndex = i;
          break;
        }
      }
      if (targetIndex >= 0) {
        this.mediaList[targetIndex].mediaSourceInfo.rect = mediaSource.mediaSourceInfo.rect;
        await mediaMixingPlugin.updateMediaSource(this.mediaList[targetIndex].mediaSourceInfo);
      }
    },
    async replaceMediaSource(srcMediaSource: TUIMediaSourceViewModel, destMediaSource: TUIMediaSourceViewModel) {
      this.checkMediaExisting(destMediaSource);

      let targetIndex = -1;
      for(let i = 0; i < this.mediaList.length; i++) {
        if (isSameMediaSource(this.mediaList[i].mediaSourceInfo, srcMediaSource.mediaSourceInfo)) {
          targetIndex = i;
          break;
        }
      }
      if (targetIndex >= 0) {
        let target = this.mediaList[targetIndex];
        if (target.mediaSourceInfo.sourceType === TUIMediaSourceType.kCamera) {
          videoEffectManager.stopEffect(target.mediaSourceInfo.sourceId);
        }
        if (!target.muted) {
          await mediaMixingPlugin.removeMediaSource(target.mediaSourceInfo);
        }

        target = {
          ...destMediaSource,
        };
        await mediaMixingPlugin.addMediaSource(target.mediaSourceInfo);
        this.mediaList[targetIndex] = target;
        
        if (target.mediaSourceInfo.sourceType === TUIMediaSourceType.kCamera) {
          videoEffectManager.stopEffect(srcMediaSource.mediaSourceInfo.sourceId);
          if (destMediaSource.beautyConfig) {
            videoEffectManager.startEffect(destMediaSource.mediaSourceInfo.sourceId, destMediaSource.beautyConfig);
          }

          if (destMediaSource.resolution) {
            await mediaMixingPlugin.setCameraCaptureParam(destMediaSource.mediaSourceInfo.sourceId, {
              mode: TRTCCameraCaptureMode.TRTCCameraCaptureManual,
              width: destMediaSource.resolution?.width,
              height: destMediaSource.resolution?.height
            });
          }
        }
      } else {
        logger.warn('[MedisSources]replaceMediaSource not found media source:', srcMediaSource);
      }
    },
    setSelectedMediaKey(selected: TUISelectedMediaKey) {
      let oldSelected = null, newSelected = null;
      for(let i = 0; i < this.mediaList.length; i++) {
        const item = this.mediaList[i];
        if (!oldSelected && this.selectedMediaKey.sourceId && isSameMediaSource(item.mediaSourceInfo, this.selectedMediaKey)) {
          item.mediaSourceInfo.isSelected = false;
          oldSelected = item;
        }
        if (!newSelected && isSameMediaSource(item.mediaSourceInfo, selected)) {
          item.mediaSourceInfo.isSelected = true;
          newSelected = item;
        }
      }

      if(oldSelected !== newSelected) {
        if (oldSelected && !oldSelected.muted) {
          mediaMixingPlugin.updateMediaSource(oldSelected.mediaSourceInfo);
        }
      }

      if (newSelected && !newSelected.muted) {
        mediaMixingPlugin.updateMediaSource(newSelected.mediaSourceInfo);
        this.selectedMediaKey = {
          ...selected
        };
      } else {
        this.selectedMediaKey = {
          sourceType: TUIMediaSourceType.kCamera,
          sourceId: '',
        };
      }
    },
    selectMediaSource(mediaSource: TUIMediaSourceViewModel) {
      this.setSelectedMediaKey(mediaSource.mediaSourceInfo);
    },
    updateBackgroundColor(color: number) {
      this.backgroundColor = color;
      mediaMixingPlugin.updatePublishParams({
        inputSourceList: this.mediaList.map(item => item.mediaSourceInfo),
        videoEncoderParams: this.mixingVideoEncodeParam,
        canvasColor: this.backgroundColor,
      });
    },
    updateResolutionMode(resMode: TRTCVideoResolutionMode) {
      this.mixingVideoEncodeParam.resMode = resMode;
      mediaMixingPlugin.updatePublishParams({
        inputSourceList: this.mediaList.map(item => item.mediaSourceInfo),
        videoEncoderParams: this.mixingVideoEncodeParam,
        canvasColor: this.backgroundColor,
      });
    },
    changeMediaOrder(mediaSource: TUIMediaSourceViewModel, changeValue: number) {
      let targetIndex = -1;
      let target = null;
      for(let i = 0; i < this.mediaList.length; i++) {
        if (isSameMediaSource(this.mediaList[i].mediaSourceInfo, mediaSource.mediaSourceInfo)) {
          targetIndex = i;
          target = this.mediaList[i];
          break;
        }
      }
      if (targetIndex >= 0 && target) {
        if (
          changeValue === 0 ||
          (targetIndex - changeValue < 0) ||
          (targetIndex - changeValue > this.mediaList.length - 1)          
        ) {
          logger.warn(`[MedisSources]changeMediaOrder invalid order change: ${changeValue}`);
          return;
        }

        if (changeValue > 0) {
          for (let i = 1; i <= changeValue; i++) {
            this.mediaList[targetIndex-i].mediaSourceInfo.zOrder = this.mediaList[targetIndex-i].mediaSourceInfo.zOrder - 1;
            if (!this.mediaList[targetIndex-i].muted) {
              mediaMixingPlugin.updateMediaSource(this.mediaList[targetIndex-i].mediaSourceInfo);
            }
            this.mediaList[targetIndex-i+1] = this.mediaList[targetIndex-i];
          }
        } else if (changeValue < 0){
          for (let i = 1; i <= (-changeValue); i++) {
            this.mediaList[targetIndex+i].mediaSourceInfo.zOrder = this.mediaList[targetIndex+i].mediaSourceInfo.zOrder + 1;
            if (!this.mediaList[targetIndex+i].muted) {
              mediaMixingPlugin.updateMediaSource(this.mediaList[targetIndex+i].mediaSourceInfo);
            }
            this.mediaList[targetIndex + i - 1] = this.mediaList[targetIndex + i];
          }
        }

        target.mediaSourceInfo.zOrder = target.mediaSourceInfo.zOrder + changeValue;
        if (!target.muted) {
          mediaMixingPlugin.updateMediaSource(target.mediaSourceInfo);
        }
        this.mediaList[targetIndex - changeValue] = target;
      } else {
        logger.warn('[MedisSources]changeMediaOrder not valid media source:', mediaSource);
      }
    },
    moveMediaTop(mediaSource: TUIMediaSourceViewModel) {
      let indexToChange = -1;
      const length = this.mediaList.length;
      for(let i = length - 1; i >= 0; i--) {
        if (indexToChange === -1 && isSameMediaSource(this.mediaList[i].mediaSourceInfo, mediaSource.mediaSourceInfo)) {
          indexToChange = i;
          break;
        }
      }
      if (indexToChange > 0) {
        for(let i = 0; i < indexToChange; i++) {
          this.mediaList[i].mediaSourceInfo.zOrder = this.mediaList[i].mediaSourceInfo.zOrder - 1;
          mediaMixingPlugin.updateMediaSource(this.mediaList[i].mediaSourceInfo);
        }
        this.mediaList[indexToChange].mediaSourceInfo.zOrder = this.mediaList.length;
        mediaMixingPlugin.updateMediaSource(this.mediaList[indexToChange].mediaSourceInfo);

        const target = this.mediaList[indexToChange];
        this.mediaList.splice(indexToChange, 1);
        this.mediaList.unshift(target);
      }
    },
    moveMediaBottom(mediaSource: TUIMediaSourceViewModel) {
      let indexToChange = -1;
      const length = this.mediaList.length;
      for(let i = 0; i < length; i++) {
        if (indexToChange === -1 && isSameMediaSource(this.mediaList[i].mediaSourceInfo, mediaSource.mediaSourceInfo)) {
          indexToChange = i;
          continue;
        }
        if (indexToChange >= 0) {
          this.mediaList[i].mediaSourceInfo.zOrder = this.mediaList[i].mediaSourceInfo.zOrder + 1;
          mediaMixingPlugin.updateMediaSource(this.mediaList[i].mediaSourceInfo);
        }
      }

      if (indexToChange >= 0 && indexToChange !== length -1) {
        const target = this.mediaList[indexToChange];
        target.mediaSourceInfo.zOrder = 1;
        mediaMixingPlugin.updateMediaSource(target.mediaSourceInfo);

        this.mediaList.splice(indexToChange, 1);
        this.mediaList.push(target);
      }
    },
    async rotateMediaSource(mediaSource: TUIMediaSourceViewModel, degree: number) {
      logger.log(`[MedisSources]rotateMediaSource:`, mediaSource, degree);
      let targetIndex = -1;
      for(let i = 0; i < this.mediaList.length; i++) {
        if (isSameMediaSource(this.mediaList[i].mediaSourceInfo, mediaSource.mediaSourceInfo)) {
          targetIndex = i;
          break;
        }
      }
      if (targetIndex >= 0) {
        const currentRotation = mediaSource.mediaSourceInfo.rotation || TRTCVideoRotation.TRTCVideoRotation0;
        const newRotation = (4 + (currentRotation as number) + degree/90) % 4;
        mediaSource.mediaSourceInfo.rotation = newRotation as TRTCVideoRotation;
        const changed = Math.abs(newRotation - currentRotation);
        if (changed === 1 || changed === 3) {
          const { left, right, top, bottom } = mediaSource.mediaSourceInfo.rect;
          mediaSource.mediaSourceInfo.rect.right = left + bottom - top;
          mediaSource.mediaSourceInfo.rect.bottom = top + right - left;
        }
        await mediaMixingPlugin.updateMediaSource(mediaSource.mediaSourceInfo);
        this.mediaList[targetIndex].mediaSourceInfo.rotation = mediaSource.mediaSourceInfo.rotation;
        this.mediaList[targetIndex].mediaSourceInfo.rect.right = mediaSource.mediaSourceInfo.rect.right;
        this.mediaList[targetIndex].mediaSourceInfo.rect.bottom = mediaSource.mediaSourceInfo.rect.bottom;
      } else {
        logger.warn('[MedisSources]rotateMediaSource not found media source:', mediaSource);
      }
    },
    async muteMediaSource(mediaSource: TUIMediaSourceViewModel, muted: boolean) {
      let targetIndex = -1;
      const length = this.mediaList.length;
      for(let i = 0; i < length; i++) {
        if (isSameMediaSource(this.mediaList[i].mediaSourceInfo, mediaSource.mediaSourceInfo)) {
          targetIndex = i;
          break;
        }
      }

      if (targetIndex >= 0) {
        const target = this.mediaList[targetIndex];
        if(muted){
          if (target.mediaSourceInfo.sourceType === TUIMediaSourceType.kCamera) {
            if (target.beautyConfig) {
              videoEffectManager.stopEffect(target.mediaSourceInfo.sourceId);
            }
          }

          await mediaMixingPlugin.removeMediaSource(target.mediaSourceInfo);
          target.muted = true;
          if (target.mediaSourceInfo.isSelected) {
            this.selectedMediaKey = {
              sourceType: TUIMediaSourceType.kCamera,
              sourceId: '',
            }
          }
        }else{
          if (this.selectedMediaKey.sourceId) {
            for(let i = 0; i < length; i++) {
              if (isSameMediaSource(this.mediaList[i].mediaSourceInfo, this.selectedMediaKey)) {
                this.mediaList[i].mediaSourceInfo.isSelected = false;
                await mediaMixingPlugin.updateMediaSource(this.mediaList[i].mediaSourceInfo);
                break;
              }
            }
          }
          
          target.mediaSourceInfo.isSelected = true;
          await mediaMixingPlugin.addMediaSource(target.mediaSourceInfo);
          target.muted = false;
          this.selectedMediaKey = {
            sourceType: target.mediaSourceInfo.sourceType,
            sourceId: target.mediaSourceInfo.sourceId,
          }

          if (target.mediaSourceInfo.sourceType === TUIMediaSourceType.kCamera) {
            if (target.resolution) {
              await mediaMixingPlugin.setCameraCaptureParam(mediaSource.mediaSourceInfo.sourceId, {
                mode: TRTCCameraCaptureMode.TRTCCameraCaptureManual,
                width: target.resolution?.width,
                height: target.resolution?.height
              });
            }
            
            if (target.beautyConfig) {
              videoEffectManager.startEffect(target.mediaSourceInfo.sourceId, target.beautyConfig);
            }
          }
        }
        logger.log(`[MedisSources]muteMediaSource:`, target, muted);
      } else {
        logger.warn('[MedisSources]muteMediaSource not found media source:', mediaSource, muted);
      }
    },
    isMediaSourceExisted(mediaSource: TUIMediaSourceViewModel): boolean {
      return this.mediaList.some(mediaItem => isSameMediaSource(mediaItem.mediaSourceInfo, mediaSource.mediaSourceInfo));
    },
    checkMediaExisting(mediaSource: TUIMediaSourceViewModel) {
      if (this.isMediaSourceExisted(mediaSource)) {
        logger.warn(`[MediaSource]addMediaSource Media source to be added already existing.:`, mediaSource);
        let deviceType = '';
        switch (mediaSource.mediaSourceInfo.sourceType) {
        case TUIMediaSourceType.kCamera:
          deviceType = t('Camera');
          break;
        case TUIMediaSourceType.kScreen:
          if (mediaSource.screenType === 0) {
            deviceType = t('Window');
          } else {
            deviceType = t('Screen');
          }
          break;
        case TUIMediaSourceType.kImage:
          deviceType = t('Image');
          break;
        default:
          logger.warn(`[MediaSource]addMediaSource unsupported media type:`, mediaSource);
          break;
        }
        throw new Error(t('Media source to be added already existing.', { deviceType: deviceType.toLocaleLowerCase() }));
      }
    },
    reset() {
      this.mediaList = [];     
    }
  },
});
