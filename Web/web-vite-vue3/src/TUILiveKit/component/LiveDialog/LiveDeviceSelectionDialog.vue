<template>
  <TUIDialog
    :title="t('Select audio and video devices')"
    :visible="modelValue"
    :custom-classes="['device-selection-dialog', `device-selection-dialog--${type}`]"
    append-to="body"
    @update:visible="handleVisibleChange"
  >
    <div class="device-selection">
      <!-- Camera preview (only for video type) -->
      <div v-if="type === DeviceSelectionType.Video" class="video-preview-container">
        <div :id="previewId" class="video-preview" />
        <div class="attention-info">
          <span
            v-if="cameraPreviewGuidance"
            class="camera-preview-failure"
          >
            <strong>{{ t(cameraPreviewGuidance.titleKey) }}</strong>
            <span>{{ t(cameraPreviewGuidance.descKey) }}</span>
          </span>
          <span
            v-else-if="cameraPreviewUnavailableGuidance"
            class="camera-preview-failure"
          >
            <strong>{{ t(cameraPreviewUnavailableGuidance.titleKey) }}</strong>
            <span>{{ t(cameraPreviewUnavailableGuidance.descKey) }}</span>
          </span>
          <span
            v-else-if="!isCameraTesting && !isCameraTestLoading"
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
      <div v-if="type === DeviceSelectionType.Video" class="device-item">
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
        <p v-if="deviceEmptyGuidance.showCamera" class="device-empty-desc">
          {{ t(emptyGuidance.descKey) }}
        </p>
      </div>
      <!-- Microphone selection (below camera selection) -->
      <div
        v-if="type === DeviceSelectionType.Video || type === DeviceSelectionType.Audio"
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
        <p v-if="deviceEmptyGuidance.showMicrophone" class="device-empty-desc">
          {{ t(emptyGuidance.descKey) }}
        </p>
      </div>
    </div>
    <template #footer>
      <div class="dialog-footer">
        <p v-if="deviceSelectionRequirementKey" class="device-selection-requirement">
          {{ t(deviceSelectionRequirementKey) }}
        </p>
        <div class="dialog-footer-actions">
          <TUIButton @click="handleCancel">{{ t('Cancel') }}</TUIButton>
          <TUIButton
            type="primary"
            :disabled="!canConfirmDeviceSelection"
            @click="handleConfirm"
          >
            {{ t('Confirm') }}
          </TUIButton>
        </div>
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
import {
  DeviceSelectionType,
  type DevicePermissionState,
  getDeviceEmptyGuidanceKeys,
  getDeviceEmptyFieldGuidance,
} from '../../utils/deviceGuidance/deviceSelectionEmptyGuidance';
import {
  CameraPreviewFailure,
  getCameraPreviewGuidanceKeys,
  getCameraPreviewUnavailableGuidanceKeys,
  getDeviceSelectionRequirementKey,
} from '../../utils/deviceGuidance/cameraPreviewGuidance';

const { t } = useUIKit();

interface Props {
  modelValue: boolean;
  type: DeviceSelectionType;
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

const emptyGuidance = getDeviceEmptyGuidanceKeys();
const cameraPermission = ref<DevicePermissionState>('unsupported');
const microphonePermission = ref<DevicePermissionState>('unsupported');
const deviceEmptyGuidance = computed(() => getDeviceEmptyFieldGuidance({
  type: props.type,
  microphoneCount: props.microphoneList.length,
  cameraCount: props.cameraList.length,
  microphonePermission: microphonePermission.value,
  cameraPermission: cameraPermission.value,
}));
const deviceSelectionRequirementKey = computed(() => getDeviceSelectionRequirementKey({
  type: props.type,
  microphoneCount: props.microphoneList.length,
  cameraCount: props.cameraList.length,
}));

const previewTRTCCloud = new TRTCCloud();
const previewId = ref<string>('');
const isCameraTesting = ref(false);
const isCameraTestLoading = ref(false);
const cameraPreviewFailure = ref<CameraPreviewFailure>(CameraPreviewFailure.None);
const cameraPreviewGuidance = computed(() => getCameraPreviewGuidanceKeys(cameraPreviewFailure.value));
const cameraPreviewUnavailableGuidance = computed(() => (
  props.modelValue
  && props.type === DeviceSelectionType.Video
  && !props.cameraId
  && !isCameraTestLoading.value
    ? getCameraPreviewUnavailableGuidanceKeys()
    : null
));

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
  if (props.type === DeviceSelectionType.Video) {
    return !!(
      props.microphoneId
      && props.cameraId
      && props.microphoneList.length
      && props.cameraList.length
    );
  }
  return !!(props.microphoneId && props.microphoneList.length);
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
  if (props.type === DeviceSelectionType.Video && previewTRTCCloud) {
    try {
      await previewTRTCCloud.setCurrentCameraDevice(newVal);
    } catch (error) {
      console.error('Failed to switch camera:', error);
      isCameraTesting.value = false;
      cameraPreviewFailure.value = CameraPreviewFailure.Switch;
    }
  }
};

async function startCameraPreview(cameraId: string) {
  isCameraTestLoading.value = true;
  isCameraTesting.value = false;
  cameraPreviewFailure.value = CameraPreviewFailure.None;

  try {
    await previewTRTCCloud.setCurrentCameraDevice(cameraId);
    const previewElement = document.getElementById(previewId.value);
    if (!previewElement) {
      throw new Error('Camera preview element not found');
    }
    await previewTRTCCloud.startCameraDeviceTest(previewElement);
    isCameraTesting.value = true;
  } catch (error) {
    console.error('Failed to start camera preview:', error);
    cameraPreviewFailure.value = CameraPreviewFailure.Preview;
  } finally {
    isCameraTestLoading.value = false;
  }
}

// Start camera preview when dialog opens and type is video
watch(
  () => [props.modelValue, props.type, props.cameraId],
  async ([visible, connectionType, cameraId]) => {
    if (visible && connectionType === DeviceSelectionType.Video && cameraId) {
      await startCameraPreview(cameraId as string);
    } else if (!visible && connectionType === DeviceSelectionType.Video) {
      // Stop preview when dialog closes
      try {
        await previewTRTCCloud.stopCameraDeviceTest();
        isCameraTesting.value = false;
        cameraPreviewFailure.value = CameraPreviewFailure.None;
      } catch (error) {
        console.error('Failed to stop camera preview:', error);
      }
    }
  },
  { immediate: true },
);

async function queryDevicePermission(name: 'camera' | 'microphone'): Promise<DevicePermissionState> {
  // Some TypeScript DOM libraries do not include media permission names.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const permissions: any = typeof navigator === 'undefined' ? null : navigator.permissions;
  if (!permissions?.query) {
    return 'unsupported';
  }
  try {
    const status = await permissions.query({ name });
    return status.state;
  } catch {
    return 'unsupported';
  }
}

watch(
  () => [props.modelValue, props.type] as const,
  async ([visible, connectionType]) => {
    if (!visible) {
      return;
    }
    const [nextMicrophonePermission, nextCameraPermission] = await Promise.all([
      queryDevicePermission('microphone'),
      connectionType === DeviceSelectionType.Video
        ? queryDevicePermission('camera')
        : Promise.resolve<DevicePermissionState>('unsupported'),
    ]);
    if (!props.modelValue || props.type !== connectionType) {
      return;
    }
    microphonePermission.value = nextMicrophonePermission;
    cameraPermission.value = nextCameraPermission;
  },
  { immediate: true },
);

watch(
  () => [props.modelValue, props.microphoneList.length] as const,
  ([visible, microphoneCount]) => {
    if (visible && microphoneCount === 0 && props.microphoneId) {
      emit('update:microphoneId', '');
    }
  },
);

// Watch camera list changes and update preview if needed
watch(
  () => props.cameraList,
  async (newList) => {
    if (!props.modelValue || props.type !== DeviceSelectionType.Video) {
      return;
    }
    if (newList.length === 0) {
      if (props.cameraId) {
        emit('update:cameraId', '');
      }
      return;
    }
    const currentCameraExists = newList.some(item => item.deviceId === props.cameraId);
    if (!currentCameraExists && newList[0]?.deviceId) {
      emit('update:cameraId', newList[0].deviceId);
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

      .camera-preview-failure {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 12px;
        max-width: 360px;
        text-align: center;
        color: var(--text-color-secondary);
        font-size: 14px;
        line-height: 20px;

        strong {
          color: var(--text-color-primary);
          font-size: 16px;
          font-weight: 500;
        }
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

  .device-empty-desc {
    margin: 0;
    font-size: 12px;
    line-height: 20px;
    color: var(--text-color-secondary);
  }
}

.dialog-footer {
  display: flex;
  align-items: center;
  justify-content: flex-end;
  gap: 12px;
  padding-top: 20px;

  .device-selection-requirement {
    flex: 1;
    margin: 0;
    color: var(--text-color-secondary);
    font-size: 12px;
    line-height: 18px;
  }

  .dialog-footer-actions {
    display: flex;
    gap: 12px;
  }
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
