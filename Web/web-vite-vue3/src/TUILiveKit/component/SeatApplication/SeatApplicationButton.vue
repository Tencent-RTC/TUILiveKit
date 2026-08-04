<template>
  <div class="apply-seat-section" @click="handleButtonClick">
    <div class="apply-seat-inner" :class="`state-${seatBtnState}`">
      <!-- apply: request co-broadcasting (two interlocked rings = two users linking) -->
      <svg
        v-if="seatBtnState === 'apply'"
        class="custom-icon" viewBox="0 0 24 24" fill="none"
        stroke-width="2" stroke-linecap="round" stroke-linejoin="round"
      >
        <path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71" stroke="#1BCFCB" />
        <path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71" stroke="#FE2C55" />
      </svg>
      <!-- pending: request sent, click to cancel (linked rings + small X = undo the request) -->
      <svg
        v-else-if="seatBtnState === 'pending'"
        class="custom-icon" viewBox="0 0 24 24" fill="none"
        stroke-width="2" stroke-linecap="round" stroke-linejoin="round"
      >
        <path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71" stroke="#1BCFCB" />
        <path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71" stroke="#FE2C55" />
        <!-- small X in the bottom-right corner to signal "undo / cancel this request" -->
        <line x1="15" y1="13" x2="19" y2="17" stroke="#FE2C55" />
        <line x1="19" y1="13" x2="15" y2="17" stroke="#FE2C55" />
      </svg>
      <!-- connected: on seat, click to leave / end the link.
           Same interlocked rings as the apply state plus a slash (top-left to
           bottom-right) to convey "the established link is cut off". -->
      <svg
        v-else
        class="custom-icon" viewBox="0 0 24 24" fill="none"
        stroke-width="2" stroke-linecap="round" stroke-linejoin="round"
      >
        <path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71" stroke="#1BCFCB" />
        <path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71" stroke="#FE2C55" />
        <!-- slash from top-left to bottom-right: cut the link -->
        <line x1="16" y1="8" x2="8" y2="16" stroke="#FE2C55" />
      </svg>
      <span class="custom-text co-guest-text">{{ applySeatBtnText }}</span>
    </div>
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
    append-to="body"
    :custom-classes="['seat-cancel-dialog']"
    :confirm-text="t('Confirm')"
    :cancel-text="t('Cancel')"
    :close="handleCancelApplicationCancel"
    :confirm="handleCancelApplicationConfirm"
    :cancel="handleCancelApplicationCancel"
  />
  <TUIDialog
    :title="t('End Link')"
    :visible="leaveSeatDialogVisible"
    append-to="body"
    :custom-classes="['seat-leave-dialog']"
    :confirm-text="t('Confirm')"
    :cancel-text="t('Cancel')"
    :close="closeLeaveSeatDialog"
    :confirm="confirmLeaveSeat"
    :cancel="closeLeaveSeatDialog"
  />
</template>
<script setup lang="ts">
import { computed, onMounted, onUnmounted, watch } from 'vue';
import {
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

// Drives the button icon + color per seat-application lifecycle stage:
//   - 'apply'    : idle viewer, action is to request co-broadcasting.
//   - 'pending'  : request already sent, action is to cancel it.
//   - 'connected': on the seat, action is to leave / end the link.
const seatBtnState = computed<'apply' | 'pending' | 'connected'>(() => {
  if (isUserOnSeat.value) {
    return 'connected';
  }
  if (isApplyingSeat.value) {
    return 'pending';
  }
  return 'apply';
});

function handleButtonClick() {
  console.log('@@@isUserOnSeat', isUserOnSeat.value);
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
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
}

.apply-seat-inner {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 7px;            // Match the like-button / more-button rhythm
  padding: 0 12px;     // Same horizontal padding as the other two pills
  background: transparent;
  border-radius: 12px;
  cursor: pointer;
  transition: transform 0.2s ease;

  // Mirror the like-button hover: the icon scales/brightens with a soft halo
  // and the label brightens. No full-area background glow.
  &:hover {
    .custom-icon {
      transform: scale(1.08);
      filter: brightness(1.08) drop-shadow(0 4px 12px rgba(255, 255, 255, 0.45));
    }

    .co-guest-text {
      color: rgba(255, 255, 255, 0.85);
    }
  }

  .custom-icon {
    width: 32px;       // Match the like-btn / more-icon slot (32×32)
    height: 32px;
    color: rgba(255, 255, 255, 0.75);
    transition: color 0.15s ease, transform 0.2s ease, filter 0.2s ease;
  }

  .co-guest-text {
    font-size: 12px;
    color: rgba(255, 255, 255, 0.55);
    transition: color 0.15s ease;
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

<!-- Non-scoped global styles. TUIDialog uses append-to="body", so the dialog
     container lives outside this component's DOM tree and scoped selectors
     (with their data-v hash) cannot reach it. These rules target the dialog
     containers by their custom-classes and align their fill with the
     GiftMorePanel frosted-glass treatment so all link-mic dialogs share one
     visual language with the rest of the glass UI. -->
<style lang="scss">
.request-connection-dialog,
.device-selection-dialog,
.seat-cancel-dialog,
.seat-leave-dialog {
  // Frosted glass moved onto a ::before pseudo-element (NOT the container
  // itself) for two reasons:
  //   1. backdrop-filter on the container creates a stacking context that
  //      traps position:fixed descendants (the TUISelect dropdown popup)
  //      and clips/relocates them — making the device select "do nothing"
  //      on click. Putting the filter on ::before leaves the container's
  //      own stacking untouched, so the dropdown can position against the
  //      viewport normally.
  //   2. ::before sits behind the container's content (z-index: -1 + the
  //      container's own flow children paint above it), so the blur still
  //      samples the same backdrop as before and the visual effect is
  //      identical to setting the filter on the container directly.
  background: transparent !important;
  backdrop-filter: none !important;
  -webkit-backdrop-filter: none !important;
  position: relative; // anchor ::before
  overflow: visible !important; // do not clip the select dropdown
  // isolation creates a stacking context WITHOUT trapping fixed descendants
  // (only transform / filter / backdrop-filter / perspective / will-change
  // do that). It keeps ::before's z-index: -1 contained instead of leaking
  // behind the page.
  isolation: isolate;
}

.request-connection-dialog::before,
.device-selection-dialog::before,
.seat-cancel-dialog::before,
.seat-leave-dialog::before {
  content: '';
  position: absolute;
  inset: 0;
  // Same frosted-glass recipe as .gift-more-panel so the dialogs share one
  // visual language with the rest of the glass UI.
  background: rgba(22, 24, 35, 0.78);
  backdrop-filter: blur(32px) saturate(1.3);
  -webkit-backdrop-filter: blur(32px) saturate(1.3);
  border-radius: inherit;
  z-index: -1;
  pointer-events: none; // let clicks reach the dialog content above
}

// ── Fix the "background flickers once before settling" on open ──────────
// TUIDialog wraps everything (mask + container) in a Vue <Transition
// name="tui-dialog-fade"> that animates `opacity 0 → 1` over 0.3s on the
// .tui-dialog-mask root. Per spec, backdrop-filter does NOT sample the
// backdrop while the element's effective opacity is < 1, so during the fade
// the ::before shows a flat rgba(22,24,35,0.78) slab; only once opacity
// hits 1 does the blur kick in — that "solid → frosted" swap is the flicker.
//
// We can't decouple the container from the mask's opacity (opacity composites
// down the subtree), so for the link-mic dialogs only we neutralise the fade:
// enter-from / leave-to stay at opacity 1, so the modal appears instantly and
// the backdrop-filter is live from the first frame. Scoped via :has() so
// other TUIDialog usages keep their fade. (:has is supported in Chrome 105+,
// which this demo targets.)
.tui-dialog-mask:has(.request-connection-dialog),
.tui-dialog-mask:has(.device-selection-dialog),
.tui-dialog-mask:has(.seat-cancel-dialog),
.tui-dialog-mask:has(.seat-leave-dialog) {
  &.tui-dialog-fade-enter-from,
  &.tui-dialog-fade-leave-to {
    opacity: 1; // skip the 0 → 1 fade so backdrop-filter is live immediately
  }
}

// TUISelect pieces inside the device-selection dialog. The base library
// fills each piece with its own solid colour (--bg-color-operate /
// --bg-color-dialog / --bg-color-topbar), which reads as three disjoint
// "dead blocks" next to the frosted glass around them. Re-tint all three
// to the same glass family and add backdrop-filter on the dropdown so
// even the popup looks like it belongs to the same modal. !important
// beats the base library's CSS-variable fills.
.device-selection-dialog .select-content {
  // Fully transparent so the dialog's own 0.78 frosted glass bleeds
  // through the trigger unchanged — the select reads as a slot cut into
  // the modal rather than a separate box painted on top. The hairline
  // border is the only visual delimiter; hover lifts it a hair so the
  // user can tell the field is interactive.
  background-color: transparent !important;
  border-color: rgba(255, 255, 255, 0.1) !important;
  &:hover {
    background-color: rgba(255, 255, 255, 0.04) !important;
  }
  &:focus {
    border-color: var(--text-color-link) !important; // keep the blue focus ring
  }
}

.device-selection-dialog .select-dropdown-container {
  // Match the dialog's frosted glass so the popup visually belongs to the
  // same modal, not a floating block from a different UI.
  background-color: rgba(22, 24, 35, 0.78) !important;
  backdrop-filter: blur(32px) saturate(1.3);
  -webkit-backdrop-filter: blur(32px) saturate(1.3);
}

.device-selection-dialog .option-container {
  // Replace the base library's solid hover/active fills with translucent
  // overlays so the frosted dropdown shows through.
  &:hover:not(.disabled) {
    background-color: rgba(255, 255, 255, 0.08) !important;
  }
  &.active {
    background-color: rgba(255, 255, 255, 0.06) !important;
  }
}
</style>
