<template>
  <div class="apply-seat-section" @click="handleButtonClick">
    <IconCoGuest class="custom-icon" />
    <span class="custom-text co-guest-text">{{ applySeatBtnText }}</span>
  </div>
  <LiveConnectionTypeDialog
    v-model="connectionTypeDialogVisible" v-model:type="requestConnectionType"
    @confirm="handleConnectionTypeConfirm" @cancel="handleConnectionTypeCancel"
  />
  <LiveDeviceSelectionDialog
    v-model="deviceSelectionDialogVisible" v-model:microphone-id="selectedMicrophoneId"
    v-model:camera-id="selectedCameraId" :type="requestConnectionType" :microphone-list="microphoneList"
    :camera-list="cameraList" @confirm="handleDeviceConfirm" @cancel="handleDeviceCancel"
  />
  <TUIDialog
    :title="t('Cancel application for link mic')"
    :visible="cancelApplicationDialogVisible"
    :confirm-text="t('Confirm')"
    :cancel-text="t('Cancel')"
    :close="handleCancelApplicationCancel"
    :confirm="handleCancelApplicationConfirm"
    :cancel="handleCancelApplicationCancel"
  />
  <TUIDialog
    :title="t('End Link')"
    :visible="leaveSeatDialogVisible"
    :confirm-text="t('Confirm')"
    :cancel-text="t('Cancel')"
    :close="closeLeaveSeatDialog"
    :confirm="confirmLeaveSeat"
    :cancel="closeLeaveSeatDialog"
  />
</template>
<script setup lang="ts">
import { onMounted, onUnmounted, watch } from 'vue';

import {
  IconCoGuest,
  TUIDialog,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';
import LiveConnectionTypeDialog from '../LiveDialog/LiveConnectionTypeDialog.vue';
import LiveDeviceSelectionDialog from '../LiveDialog/LiveDeviceSelectionDialog.vue';
import { useSeatApplication } from './useSeatApplication';

const { t } = useUIKit();

const {
  isUserOnSeat,
  isApplyingSeat,
  applySeatBtnText,
  connectionTypeDialogVisible,
  deviceSelectionDialogVisible,
  cancelApplicationDialogVisible,
  leaveSeatDialogVisible,
  requestConnectionType,
  selectedMicrophoneId,
  selectedCameraId,
  microphoneList,
  cameraList,
  handleApplyForSeat,
  openLeaveSeatDialog,
  confirmLeaveSeat,
  closeLeaveSeatDialog,
  handleCancelApplicationOnSeat,
  handleConnectionTypeConfirm,
  handleConnectionTypeCancel,
  handleDeviceConfirm,
  handleDeviceCancel,
  handleCancelApplicationConfirm,
  handleCancelApplicationCancel,
  initAutoSelectDevice,
  subscribeEvents,
  unsubscribeEvents,
} = useSeatApplication();

function handleButtonClick() {
  if (isUserOnSeat.value) {
    openLeaveSeatDialog();
  } else if (isApplyingSeat.value) {
    handleCancelApplicationOnSeat();
  } else {
    handleApplyForSeat();
  }
}

watch(deviceSelectionDialogVisible, (val) => {
  if (val) {
    initAutoSelectDevice();
  }
});

onMounted(() => {
  subscribeEvents();
});

onUnmounted(() => {
  unsubscribeEvents();
});

</script>
<style scoped lang="scss">
.apply-seat-section {
  width: 120px;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;

  &:hover {
    cursor: pointer;
  }

  .custom-icon {
    width: 36px;
    height: 36px;
    margin-bottom: 10px;
  }

  .co-guest-text {
    font-size: 14px;
  }
}

.connection-options {
  padding: 0;

  .options-section {
    width: 100%;

    .section-label {
      font-size: 14px;
      color: var(--text-color-primary);
      margin: 0 0 16px 0;
      font-weight: 500;
    }

    .options-grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 16px;

      .option-card {
        box-sizing: border-box;
        padding: 16px;
        background: var(--bg-color-function);
        border: 2px solid transparent;
        border-radius: 12px;
        cursor: pointer;
        transition: all 0.2s ease;
        text-align: left;

        &:hover {
          background: var(--list-color-hover);
          border-color: var(--stroke-color-module);
        }

        &.active {
          border: 2px solid var(--text-color-link-hover);
          background: var(--list-color-focused);

          .option-info {
            .option-icon {
              color: var(--text-color-link-hover);
            }

            h4 {
              color: var(--text-color-primary);
              font-weight: 600;
            }
          }
        }

        .option-info {
          display: flex;
          align-items: center;
          justify-content: flex-start;
          gap: 12px;

          .option-icon {
            width: 24px;
            height: 24px;
            color: var(--text-color-secondary);
            transition: color 0.2s ease;
            flex-shrink: 0;
          }

          h4 {
            margin: 0;
            font-size: 14px;
            font-weight: 400;
            color: var(--text-color-primary);
            transition: color 0.2s ease;
          }
        }
      }
    }
  }
}

.dialog-footer {
  display: flex;
  justify-content: flex-end;
  gap: 12px;
  padding-top: 20px;
}

:deep(.request-connection-dialog) {
  .tui-dialog__body {
    padding: 24px;
  }
}

.device-selection {
  padding: 20px 0;
  display: flex;
  flex-direction: column;
  gap: 20px;

  .device-item {
    display: flex;
    flex-direction: column;
    gap: 12px;

    .device-label {
      font-size: 14px;
      color: var(--text-color-secondary);
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

:deep(.device-selection-dialog) {
  .tui-dialog__body {
    padding: 24px;
  }
}
</style>
