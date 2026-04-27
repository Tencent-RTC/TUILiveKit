<template>
  <div class="device-setting">
    <AudioIcon
      :size="16"
      :audio-volume="currentMicVolume"
      :is-muted="microphoneStatus === DeviceStatus.Off"
      @click="switchMicrophoneStatus"
    />
    <TUISlider
      :model-value="displayVolume"
      class="device-slider"
      :min="0"
      :max="100"
      @change="handleMicrophoneVolumeChange"
    />
  </div>
</template>

<script lang="ts" setup>
import { computed, ref, watch } from 'vue';
import { TUISlider, TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { DeviceError, DeviceStatus, useDeviceState } from 'tuikit-atomicx-vue3';
import AudioIcon from '../base-component/AudioIcon.vue';

const { t } = useUIKit();

const {
  setCaptureVolume,
  microphoneStatus,
  muteLocalAudio,
  unmuteLocalAudio,
  openLocalMicrophone,
  microphoneLastError,
  currentMicVolume,
} = useDeviceState();

const DEFAULT_VOLUME = 100;

// User-intended capture volume. Persisted at MODULE scope (not component
// scope) so that it survives MicVolumeSetting's unmount/remount cycle
// across "end live -> back to list -> start live again". Stored value is
// ALWAYS a non-zero number in [1, 100] — it represents what the user wants
// when the mic is on. Muting the mic does NOT overwrite this value, only
// sets the SDK captureVolume to 0 while leaving `intendedVolume` intact.
//
// Why module-scope: in the previous implementation `microphoneVolumeBeforeMute`
// was a component ref initialized from `captureVolume.value`. Because
// `captureVolume` (module-scope ref in DeviceState) got written to 0 during
// the first live session, on remount the component read 0 back, effectively
// losing the user's intended volume and causing the second-time-open
// "observers hear nothing" regression.
const intendedVolume = ref(DEFAULT_VOLUME);

// Slider display value is DERIVED from mic status + intended volume.
// When the mic is off, the slider shows 0 (UI consistency with the muted
// icon). When on, it shows the user's intended volume. Writing to the
// slider is handled explicitly in `handleMicrophoneVolumeChange`.
const displayVolume = computed(() =>
  (microphoneStatus.value === DeviceStatus.Off ? 0 : intendedVolume.value),
);

const handleMicrophoneVolumeChange = async (value: number) => {
  if (value > 0) {
    // User set a non-zero volume: record it as the new intended volume
    // and ensure the mic is on at that volume.
    intendedVolume.value = value;
    if (microphoneStatus.value === DeviceStatus.Off) {
      try {
        await openLocalMicrophone();
        await unmuteLocalAudio();
      } catch (err) {
        console.warn('[MicVolumeSetting] openLocalMicrophone failed:', err);
      }
    }
    await setCaptureVolume(value);
  } else {
    // User dragged the slider to 0: mute the mic and zero the SDK volume,
    // but PRESERVE `intendedVolume` so the next unmute restores it.
    if (microphoneStatus.value === DeviceStatus.On) {
      await muteLocalAudio();
    }
    await setCaptureVolume(0);
  }
};

const switchMicrophoneStatus = async () => {
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
    // Mute without touching `intendedVolume`.
    await muteLocalAudio();
    await setCaptureVolume(0);
  } else {
    // Unmute and restore the user's intended volume.
    try {
      await openLocalMicrophone();
      await unmuteLocalAudio();
    } catch (err) {
      console.warn('[MicVolumeSetting] openLocalMicrophone failed:', err);
    }
    await setCaptureVolume(intendedVolume.value || DEFAULT_VOLUME);
  }
};

// When the mic transitions from Off -> On (e.g. triggered by LivePusherView's
// `openLocalMicrophone` after startLive success, or by any other code path),
// defensively re-apply the user's intended volume to the SDK. This protects
// against trtc-sdk-v5 inheriting a stale captureVolume=0 from a previous
// stopLocalAudio, which would otherwise silence the host on second-time-open.
watch(microphoneStatus, (next, prev) => {
  if (next === DeviceStatus.On && prev === DeviceStatus.Off) {
    const volume = intendedVolume.value || DEFAULT_VOLUME;
    setCaptureVolume(volume);
  }
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
