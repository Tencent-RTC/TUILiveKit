<template>
  <div class="device-setting">
    <AudioIcon
      :size="16"
      :audioVolume="currentMicVolume"
      :isMuted="microphoneStatus === DeviceStatus.Off"
      @click="switchMicrophoneStatus"
    />
    <TUISlider
      v-model="microphoneVolume"
      class="device-slider"
      :min="0"
      :max="100"
      @change="handleMicrophoneVolumeChange"
      @mouseup="saveMicrophoneVolumeBeforeMute"
    />

  </div>
</template>

<script lang="ts" setup>
import { ref, watch } from 'vue';
import { TUISlider, TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { DeviceError, DeviceStatus, useDeviceState, useLiveListState } from 'tuikit-atomicx-vue3';
import AudioIcon from '../base-component/AudioIcon.vue';

const { t } = useUIKit();

const {
  captureVolume,
  setCaptureVolume,
  microphoneStatus,
  openLocalMicrophone,
  closeLocalMicrophone,
  microphoneLastError,
  currentMicVolume,
} = useDeviceState();
const { currentLive } = useLiveListState();

const DEFAULT_VOLUME = 100;
const microphoneVolume = ref(captureVolume.value);
const microphoneVolumeBeforeMute = ref(captureVolume.value);

// To ensure that when the initial microphone is in the off state, the capture volume is also 0, which is consistent on the ui
setCaptureVolume(0);

const handleMicrophoneVolumeChange = async (value: number) => {
  if (value !== captureVolume.value) {
    await setCaptureVolume(value);
  }

  if (value === 0 && microphoneStatus.value === DeviceStatus.On) {
    await closeLocalMicrophone();
  }

  if (value > 0 && microphoneStatus.value === DeviceStatus.Off) {
    await openLocalMicrophone();
  }
};

const saveMicrophoneVolumeBeforeMute = () => {
  if (microphoneVolume.value > 0) {
    microphoneVolumeBeforeMute.value = microphoneVolume.value;
  }
};

const switchMicrophoneStatus = () => {
  if (microphoneLastError.value !== DeviceError.NoError) {
    switch (microphoneLastError.value) {
      case DeviceError.NoDeviceDetected:
        TUIToast.error({
          message: t('No device detected'),
        });
        break;
      case DeviceError.NoSystemPermission:
        TUIToast.error({
          message: t('No system permission'),
        });
        break;
      case DeviceError.NotSupportCapture:
        TUIToast.error({
          message: t('Not support capture'),
        });
        break;
      default:
        break;
    }
  }
  if (microphoneStatus.value === DeviceStatus.On) {
    microphoneVolumeBeforeMute.value = microphoneVolume.value || DEFAULT_VOLUME;
    setCaptureVolume(0);
    closeLocalMicrophone();
  } else {
    const volumeToRestore = microphoneVolumeBeforeMute.value || DEFAULT_VOLUME;
    setCaptureVolume(volumeToRestore);
    openLocalMicrophone();
  }
};

watch(captureVolume, (newVal) => {
  microphoneVolume.value = newVal;
});

watch(() => currentLive.value?.liveId, async (newVal) => {
  if (newVal) {
    setCaptureVolume(microphoneVolumeBeforeMute.value);
  } else {
    microphoneVolumeBeforeMute.value = microphoneVolume.value;
    setCaptureVolume(0);
  }
}, {
  immediate: true,
});
</script>

<style lang="scss" scoped>
@import '../style/index.scss';

.device-setting {
  display: flex;
  align-items: center;
  gap: 8px;
  background-color: var(--bg-color-bubble-reciprocal);
  padding: 0 8px;
  border-radius: 6px;
  height: 40px;

  .device-slider {
    flex: 1;
    width: 46px;

    :deep(.slider-thumb) {
      width: 8px;
      height: 8px;
    }

    :deep(.slider-thumb-disabled) {
      border-color: var(--slider-color-empty);
    }
  }
}
</style>
