<template>
  <TUIDialog
    :title="t('Select audio and video devices')"
    :visible="modelValue"
    :custom-classes="['device-selection-dialog', `device-selection-dialog--${type}`]"
    @update:visible="handleVisibleChange"
  >
    <div class="device-selection">
      <!-- Camera preview (only for video type) -->
      <div v-if="type === 'video'" class="video-preview-container">
        <div :id="previewId" class="video-preview" />
        <div class="attention-info">
          <span
            v-if="!isCameraTesting && !isCameraTestLoading"
            class="off-camera-info"
          >{{ t('Off Camera') }}
          </span>
          <IconLoading
            v-if="isCameraTestLoading"
            size="36"
            class="loading"
          />
        </div>
      </div>
      <!-- Camera selection (only for video type, below preview) -->
      <div v-if="type === 'video'" class="device-item">
        <span class="device-label">{{ t('Camera') }}</span>
        <TUISelect
          v-model="localCameraId"
          class="device-select"
          :disabled="cameraList.length === 0"
          @change="handleCameraChange"
        >
          <TUIOption
            v-for="item in cameraList"
            :key="item.deviceId"
            :value="item.deviceId"
            :label="item.deviceName"
          />
        </TUISelect>
        <div v-if="cameraList.length === 0" class="device-empty-tip">
          {{ t('No camera device available') }}
        </div>
      </div>
      <!-- Microphone selection (below camera selection) -->
      <div
        v-if="type === 'video' || type === 'audio'"
        class="device-item"
      >
        <span class="device-label">{{ t('Microphone') }}</span>
        <TUISelect
          v-model="localMicrophoneId"
          class="device-select"
          :disabled="microphoneList.length === 0"
        >
          <TUIOption
            v-for="item in microphoneList"
            :key="item.deviceId"
            :value="item.deviceId"
            :label="item.deviceName"
          />
        </TUISelect>
        <div v-if="microphoneList.length === 0" class="device-empty-tip">
          {{ t('No microphone device available') }}
        </div>
      </div>
    </div>
    <template #footer>
      <div class="dialog-footer">
        <TUIButton @click="handleCancel">{{ t('Cancel') }}</TUIButton>
        <TUIButton
          type="primary"
          :disabled="!canConfirmDeviceSelection"
          @click="handleConfirm"
        >
          {{ t('Confirm') }}
        </TUIButton>
      </div>
    </template>
  </TUIDialog>
</template>

<script setup lang="ts">
import { computed, ref, watch, onBeforeUnmount, onBeforeMount } from 'vue';
import type { TUIDeviceInfo } from '@tencentcloud/tuiroom-engine-js';
import { TRTCCloud } from '@tencentcloud/tuiroom-engine-js';
import {
  TUIDialog,
  TUIButton,
  TUISelect,
  TUIOption,
  IconLoading,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';

const { t } = useUIKit();

interface Props {
  modelValue: boolean;
  type: 'video' | 'audio';
  microphoneList: TUIDeviceInfo[];
  cameraList: TUIDeviceInfo[];
  microphoneId: string;
  cameraId: string;
}

interface Emits {
  (e: 'update:modelValue', value: boolean): void;
  (e: 'update:microphoneId', value: string): void;
  (e: 'update:cameraId', value: string): void;
  (e: 'confirm'): void;
  (e: 'cancel'): void;
}

const props = defineProps<Props>();
const emit = defineEmits<Emits>();

const previewTRTCCloud = new TRTCCloud();
const previewId = ref<string>('');
const isCameraTesting = ref(false);
const isCameraTestLoading = ref(false);

onBeforeMount(() => {
  previewId.value = `live-device-preview-${Math.random().toString(36)
    .substring(2, 15)}`;
});

const localMicrophoneId = computed({
  get: () => props.microphoneId,
  set: (val: string) => emit('update:microphoneId', val),
});

const localCameraId = computed({
  get: () => props.cameraId,
  set: (val: string) => emit('update:cameraId', val),
});

const canConfirmDeviceSelection = computed(() => {
  if (props.type === 'video') {
    return !!(props.microphoneId && props.cameraId);
  }
  return !!props.microphoneId;
});

const handleVisibleChange = (visible: boolean) => {
  emit('update:modelValue', visible);
};

const handleCancel = () => {
  emit('cancel');
  emit('update:modelValue', false);
};

const handleConfirm = () => {
  if (!canConfirmDeviceSelection.value) {
    return;
  }
  emit('confirm');
};

const handleCameraChange = async (newVal: string) => {
  if (props.type === 'video' && previewTRTCCloud) {
    try {
      await previewTRTCCloud.setCurrentCameraDevice(newVal);
    } catch (error) {
      console.error('Failed to switch camera:', error);
    }
  }
};

// Start camera preview when dialog opens and type is video
watch(
  () => [props.modelValue, props.type, props.cameraId],
  async ([visible, connectionType, cameraId]) => {
    if (visible && connectionType === 'video' && cameraId) {
      try {
        isCameraTestLoading.value = true;
        await previewTRTCCloud.setCurrentCameraDevice(cameraId);
        await previewTRTCCloud.startCameraDeviceTest(previewId.value);
        isCameraTesting.value = true;
        isCameraTestLoading.value = false;
      } catch (error) {
        console.error('Failed to start camera preview:', error);
        isCameraTestLoading.value = false;
      }
    } else if (!visible && connectionType === 'video') {
      // Stop preview when dialog closes
      try {
        await previewTRTCCloud.stopCameraDeviceTest();
        isCameraTesting.value = false;
      } catch (error) {
        console.error('Failed to stop camera preview:', error);
      }
    }
  },
  { immediate: true },
);

// Watch camera list changes and update preview if needed
watch(
  () => props.cameraList,
  async (newList) => {
    if (props.modelValue && props.type === 'video' && newList.length > 0) {
      const currentCameraExists = newList.some(item => item.deviceId === props.cameraId);
      if (!currentCameraExists && newList[0]?.deviceId) {
        // Update camera ID if current one is not in the list
        emit('update:cameraId', newList[0].deviceId);
      }
    }
  },
);

onBeforeUnmount(async () => {
  try {
    await previewTRTCCloud.stopCameraDeviceTest();
    previewTRTCCloud.destroy();
  } catch (error) {
    console.error('Failed to cleanup camera preview:', error);
  }
});
</script>

<style scoped lang="scss">
.device-selection {
  padding: 20px 0;
  display: flex;
  flex-direction: column;
  gap: 20px;
  width: 100%;

  .video-preview-container {
    position: relative;
    width: 100%;
    height: 300px;
    overflow: hidden;
    background-color: var(--uikit-color-black-1);
    border-radius: 8px;
    margin-bottom: 4px;

    .video-preview {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }

    .attention-info {
      position: absolute;
      top: 0;
      left: 0;
      display: flex;
      align-items: center;
      justify-content: center;
      width: 100%;
      height: 100%;

      .off-camera-info {
        font-size: 22px;
        font-weight: 400;
        line-height: 34px;
        color: var(--text-color-secondary);
      }

      .loading {
        animation: loading-rotate 2s linear infinite;
      }
    }
  }

  .device-item {
    display: flex;
    flex-direction: column;
    gap: 12px;

    .device-label {
      font-size: 14px;
      color: var(--text-color-primary);
      font-weight: 500;
    }

    .device-select {
      width: 100%;
    }

    .device-empty-tip {
      font-size: 12px;
      color: var(--text-color-secondary);
      margin-top: -8px;
    }
  }
}

.dialog-footer {
  display: flex;
  justify-content: flex-end;
  gap: 12px;
  padding-top: 20px;
}

:deep(.device-selection-dialog) {
  width: 500px;

  &.device-selection-dialog--video {
    width: 600px;
  }

  .tui-dialog__body {
    padding: 24px;
  }
}

@keyframes loading-rotate {
  0% {
    transform: rotate(0deg);
  }

  100% {
    transform: rotate(360deg);
  }
}
</style>
