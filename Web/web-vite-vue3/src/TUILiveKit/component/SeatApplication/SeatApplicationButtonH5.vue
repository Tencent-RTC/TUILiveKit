<template>
  <div
    class="apply-seat-h5"
    :class="{ 'is-active': isUserOnSeat, 'is-applying': isApplyingSeat }"
    @click="handleButtonClick"
  >
    <IconCallVoice v-if="isUserOnSeat" size="20" />
    <IconCoGuest v-else size="20" />
  </div>
  <LiveConnectionTypeDrawerH5
    v-model:visible="connectionTypeDialogVisible"
    v-model:type="requestConnectionType"
    :audio-only="connectionTypeDrawerAudioOnly"
    @confirm="handleConnectionTypeConfirm"
    @open-settings="handleOpenVideoAdjust"
  />
  <LivePermissionPrimerDrawerH5
    v-model:visible="permissionPrimerVisible"
    :mode="permissionPrimerMode"
    :type="requestConnectionType"
    @confirm="handlePermissionPrimerConfirm"
    @cancel="handlePermissionPrimerCancel"
  />
  <LiveVideoAdjustDrawerH5
    v-model:visible="videoAdjustVisible"
    @apply="handleVideoAdjustApply"
  />
  <transition name="action-sheet-slide">
    <div v-if="cancelApplicationDialogVisible" class="action-sheet-mask" @click="handleCancelApplicationCancel">
      <div class="action-sheet" @click.stop>
        <div class="action-sheet-btn action-sheet-btn-danger" @click="handleCancelApplicationConfirm">
          {{ t('Cancel application for link mic') }}
        </div>
        <div class="action-sheet-btn action-sheet-btn-cancel" @click="handleCancelApplicationCancel">
          {{ t('Cancel') }}
        </div>
      </div>
    </div>
  </transition>
  <transition name="action-sheet-slide">
    <div v-if="leaveSeatDialogVisible" class="action-sheet-mask" @click="closeLeaveSeatDialog">
      <div class="action-sheet" @click.stop>
        <div class="action-sheet-btn action-sheet-btn-danger" @click="confirmLeaveSeat">
          {{ t('End Link') }}
        </div>
        <div class="action-sheet-btn action-sheet-btn-cancel" @click="closeLeaveSeatDialog">
          {{ t('Cancel') }}
        </div>
      </div>
    </div>
  </transition>
</template>

<script setup lang="ts">
import { ref, onMounted, onUnmounted } from 'vue';
import {
  IconCoGuest,
  IconCallVoice,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';
import LiveConnectionTypeDrawerH5 from '../LiveDialog/LiveConnectionTypeDrawerH5.vue';
import LivePermissionPrimerDrawerH5 from '../LiveDialog/LivePermissionPrimerDrawerH5.vue';
import LiveVideoAdjustDrawerH5 from '../LiveDialog/LiveVideoAdjustDrawerH5.vue';
import { useSeatApplication } from './useSeatApplication';

const { t } = useUIKit();

const videoAdjustVisible = ref(false);

// H5 path: skips the device-selection dialog after picking the connection type.
const {
  isUserOnSeat,
  isApplyingSeat,
  connectionTypeDialogVisible,
  connectionTypeDrawerAudioOnly,
  cancelApplicationDialogVisible,
  leaveSeatDialogVisible,
  permissionPrimerVisible,
  permissionPrimerMode,
  requestConnectionType,
  handleApplyForSeat,
  openLeaveSeatDialog,
  confirmLeaveSeat,
  closeLeaveSeatDialog,
  handleCancelApplicationOnSeat,
  handleConnectionTypeConfirm,
  handleCancelApplicationConfirm,
  handleCancelApplicationCancel,
  handlePermissionPrimerConfirm,
  handlePermissionPrimerCancel,
  subscribeEvents,
  unsubscribeEvents,
} = useSeatApplication('h5');

function handleButtonClick() {
  if (isUserOnSeat.value) {
    openLeaveSeatDialog();
  } else if (isApplyingSeat.value) {
    handleCancelApplicationOnSeat();
  } else {
    handleApplyForSeat();
  }
}

function handleOpenVideoAdjust() {
  connectionTypeDialogVisible.value = false;
  videoAdjustVisible.value = true;
}

function handleVideoAdjustApply() {
  videoAdjustVisible.value = false;
  requestConnectionType.value = 'video';
  handleConnectionTypeConfirm();
}

onMounted(() => {
  subscribeEvents();
});

onUnmounted(() => {
  unsubscribeEvents();
});
</script>

<style scoped lang="scss">
.apply-seat-h5 {
  width: 32px;
  height: 32px;
  border-radius: 50%;
  background-color: var(--text-color-link, #4086FF);
  color: var(--text-color-button);
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  transition: all 0.2s ease;
  -webkit-tap-highlight-color: transparent;
  outline: none;
  user-select: none;

  &:active {
    transform: scale(0.95);
    opacity: 0.9;
  }

  &.is-applying {
    background-color: var(--button-color-accept, #2BA471);
  }

  &.is-active {
    background-color: var(--button-color-hangup, #E54545);
  }
}

.action-sheet-mask {
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  background: var(--bg-color-mask);
  z-index: 1100;
  display: flex;
  align-items: flex-end;
  justify-content: center;
}

.action-sheet {
  width: 100%;
  padding-bottom: env(safe-area-inset-bottom, 0);
  border-top-left-radius: 12px;
  border-top-right-radius: 12px;
  overflow: hidden;
}

.action-sheet-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  height: 56px;
  font-size: 16px;
  font-weight: 500;
  cursor: pointer;
  -webkit-tap-highlight-color: transparent;
}

.action-sheet-btn-danger {
  background: var(--uikit-color-gray-2, #2A2D33);
  color: var(--button-color-hangup, #E54545);
}

.action-sheet-btn-cancel {
  background: var(--uikit-color-gray-2, #2A2D33);
  color: var(--text-color-secondary, rgba(255, 255, 255, 0.6));
  border-top: 1px solid var(--stroke-color-secondary);
}

.action-sheet-slide-enter-active,
.action-sheet-slide-leave-active {
  transition: opacity 0.3s ease;

  .action-sheet {
    transition: transform 0.3s cubic-bezier(0.4, 0, 0.2, 1);
  }
}

.action-sheet-slide-enter-from,
.action-sheet-slide-leave-to {
  opacity: 0;

  .action-sheet {
    transform: translateY(100%);
  }
}
</style>
