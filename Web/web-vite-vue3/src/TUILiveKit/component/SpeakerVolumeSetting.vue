<template>
  <div class="device-setting">
    <TUIIcon
      class="device-icon"
      :icon="speakerIsOn ? IconSpeakerOn : IconSpeakerOff"
      @click="switchSpeaker(!speakerIsOn)"
    />
    <TUISlider
      v-model="speakerVolume"
      class="device-slider"
      :min="0"
      :max="100"
      @change="handleSpeakerVolumeChange"
      @mouseup="saveSpeakerVolumeBeforeMute"
    />
  </div>
</template>

<script lang="ts" setup>
import { ref, watch } from 'vue';
import { TUIIcon, TUISlider, IconSpeakerOn, IconSpeakerOff } from '@tencentcloud/uikit-base-component-vue3';
import { useDeviceState } from 'tuikit-atomicx-vue3';

const { outputVolume, setOutputVolume } = useDeviceState();

const DEFAULT_VOLUME = 100;
const speakerVolume = ref(outputVolume.value);
const speakerIsOn = ref(true);
const speakerVolumeBeforeMute = ref(outputVolume.value);

const saveSpeakerVolumeBeforeMute = () => {
  if (speakerVolume.value > 0) {
    speakerVolumeBeforeMute.value = speakerVolume.value;
  }
};

const switchSpeaker = (open: boolean) => {
  speakerIsOn.value = open;
  if (!open) {
    speakerVolumeBeforeMute.value = speakerVolume.value || DEFAULT_VOLUME;
    speakerVolume.value = 0;
    setOutputVolume(0);
  } else {
    const volumeToRestore = speakerVolumeBeforeMute.value || DEFAULT_VOLUME;
    speakerVolume.value = volumeToRestore;
    setOutputVolume(volumeToRestore);
  }
};

const handleSpeakerVolumeChange = (value: number) => {
  if (value !== outputVolume.value) {
    setOutputVolume(value);
  }

  if (value === 0 && speakerIsOn.value) {
    speakerIsOn.value = false;
  }

  if (value > 0 && !speakerIsOn.value) {
    speakerIsOn.value = true;
  }
};

watch(outputVolume, (newVal) => {
  speakerVolume.value = newVal;
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

  .device-icon {
    width: 24px;
    height: 24px;
    cursor: pointer;
  }
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
