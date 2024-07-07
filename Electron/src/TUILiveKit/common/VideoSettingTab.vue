<!--
  * Name: VideoTab
  * @param name String required
  * @param size String 'large'|'medium'|'small'
  * Usage:
  * Use <video-tab></video-tab> in the template
  *
  * 名称: VideoTab
  * @param name String required
  * @param size String 'large'|'medium'|'small'
  * 使用方式：
  * 在 template 中使用 <video-tab></video-tab>
-->
<template>
  <div class="video-tab">
    <div class="tui-camera-preview" ref="cameraPreviewRef"></div>
    <div class="camera-setting-container">
      <div class="camera-setting">
        <span class="title">{{ t('Camera') }}</span>
        <device-select device-type="camera"></device-select>
      </div>
      <div class="resolution-setting">
        <span class="title">{{ t('Resolution') }}</span>
        <video-profile></video-profile>
      </div>
      <div class="mirror-container">
        <span @click="handleChangeMirror">
          <CameraMirror v-if="isCurrentCameraMirrored"/>
          <CameraUnmirror v-else />
        </span>
      </div>
    </div>
    <div class="beauty-setting" v-if="props.withBeauty">
      <BeautyConfigPanel :init-value="beautyProperties" @on-change="handleBeautyEffectChange" />
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, Ref, defineProps, onMounted, onBeforeUnmount } from 'vue';
import { storeToRefs } from 'pinia';
import DeviceSelect from './DeviceSelect.vue';
import VideoProfile from './VideoProfile.vue';
import CameraMirror from '../common/icons/CameraMirror.vue';
import CameraUnmirror from '../common/icons/CameraUnmirror.vue';
import BeautyConfigPanel from '../common/BeautyConfigPanel.vue';
import { useI18n } from '../locales/index';
import { useCurrentSourcesStore } from '../store/currentSources';
import { TRTCXmagicEffectProperty } from '../utils/beauty';
// import useGetRoomEngine from '../utils/useRoomEngine';
// import { TRTCVideoFillMode, TRTCVideoMirrorType, TRTCVideoRotation } from '@tencentcloud/tuiroom-engine-electron';
// const roomEngine = useGetRoomEngine();
const { t } = useI18n()
const sourcesStore = useCurrentSourcesStore();
const { isCurrentCameraMirrored, beautyProperties } = storeToRefs(sourcesStore);
interface Props {
  withBeauty?: boolean;
}
const logger = console;
const logPrefix = '[VideoSettingTab]';
const props = defineProps<Props>();

const cameraPreviewRef: Ref<HTMLDivElement | undefined> = ref();
const handleChangeMirror = async () => {
  logger.log(`${logPrefix}handleChangeMirror: ${isCurrentCameraMirrored.value}`);
  sourcesStore.setIsCurrentCameraMirrored(!isCurrentCameraMirrored.value);
  window.mainWindowPort?.postMessage({
    key: "setCameraTestRenderMirror",
    data: {
      mirror: isCurrentCameraMirrored.value
    }
  });
}

function startCameraPreview() {
  logger.log(`${logPrefix}startCameraPreview`, cameraPreviewRef.value, window.nativeWindowHandle);
  setTimeout(() => {
    if (cameraPreviewRef.value && window.nativeWindowHandle) {
      const bodyRect = document.body.getBoundingClientRect();
      const clientRect = cameraPreviewRef.value.getBoundingClientRect();
      window.mainWindowPort?.postMessage({
        key: "startCameraDeviceTest",
        data: {
          windowID: window.nativeWindowHandle,
          rect: {
            left: Math.round(clientRect.left * window.devicePixelRatio),
            right: Math.round(clientRect.right * window.devicePixelRatio),
            top: Math.round(clientRect.top * window.devicePixelRatio),
            bottom: Math.round(clientRect.bottom * window.devicePixelRatio),
          },
          log: `-----startCameraPreview
            body area left:${bodyRect.left} right:${bodyRect.right} top:${bodyRect.top} bottom: ${bodyRect.bottom}
            view area left:${clientRect.left} right:${clientRect.right} top:${clientRect.top} bottom: ${clientRect.bottom}
            devicePixelRatio: ${window.devicePixelRatio}`
        }
      });
    } else {
      logger.error(`${logPrefix}Preview camera failed, not DIV view or native window ID.`, cameraPreviewRef.value, window.nativeWindowHandle);
    }
  }, 100);
}

function stopCameraPreview() {
  logger.debug(`${logPrefix}stopCameraPreview`);
  window.mainWindowPort?.postMessage({
    key: "stopCameraDeviceTest"
  });
  logger.log(`${logPrefix}stopCameraPreview finished`);
}

function handleBeautyEffectChange(properties: TRTCXmagicEffectProperty[]) {
  logger.log(`${logPrefix}handleBeautyEffectChange:`, properties);
  window.mainWindowPort?.postMessage({
    key: "setCameraTestVideoPluginParameter",
    data: properties
  });
  sourcesStore.setBeautyProperties(properties);
}

onMounted(() => {
  logger.log(`${logPrefix}onMounted`);
  window.mainWindowPort?.postMessage({
    key: "setCameraTestVideoPluginPath",
    data: true
  });
  startCameraPreview();
});

onBeforeUnmount(() => {
  logger.log(`${logPrefix}onBeforeUnmount`);
  stopCameraPreview();
  window.mainWindowPort?.postMessage({
    key: "setCameraTestVideoPluginPath",
    data: false
  });
});
</script>

<style lang="scss" scoped>
.camera-setting-container{
  width: 100%;
  display: flex;
  height: 4.625rem;
  padding: 0.5rem 0;
}
.camera-setting{
  width: 20rem;
}
.resolution-setting{
    width: 12rem;
    padding-left: 0.75rem;
}
.beauty-setting{
  width: 100%;
  height: calc(100% - 21.625rem);
}
.video-tab {
    width: 100%;
    height: 100%;
    display: flex;
    flex-wrap: wrap;
    justify-content: space-between;
  }
  .title {
    font-size: 0.75rem;
    font-style: normal;
    font-weight: 400;
    line-height: 1rem;
  }
  .video-preview-container {
    position: relative;
    width: 100%;
    height: 0;
    padding-top: calc(100% * 9 / 16);
    background-color: #000000;
    border-radius: 0.5rem;
    overflow: hidden;
    .video-preview {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }
  }
  .tui-camera-preview{
    width: 100%;
    height: 17rem;
    overflow: hidden;
}
  .mirror-container {
    display: flex;
    align-self: end;
    padding-left: 0.75rem;
    font-size: 0;
    cursor: pointer;
  }
  .item {
    width: 100%;
    height: 1.25rem;
    cursor: pointer;
    color: var(--font-color-3);
  }
</style>
