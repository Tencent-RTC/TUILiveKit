<template>
  <div id="liveContainer" ref="liveContainerRef" class="live-player-education-pc">
    <section class="edu-layout">
      <!-- Header bar: spans full width, title + status capsule -->
      <header class="edu-header-bar">
        <div class="header-left">
          <div class="edu-scene-brand">
            <img class="edu-scene-brand-logo" src="../../../assets/imgs/logo.svg" alt="Tencent RTC" />
            <span class="edu-scene-brand-tag">{{ t('Education') }}</span>
          </div>
          <h1 class="course-title">{{ displayRoomTitle }}</h1>
        </div>
        <div class="header-right">
          <!-- Separate status capsules for live indicator -->
          <div class="status-capsule live-capsule">
            <span class="live-dot" />
            <span class="live-text">{{ t('LiveStreaming') }}</span>
          </div>
          <button class="header-exit-btn" :title="t('Exit')" @click="handleLeaveLive">
            <svg class="header-exit-icon" viewBox="0 0 24 24" fill="none">
              <path d="M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4" />
              <path d="M10 17l5-5-5-5" />
              <path d="M15 12H3" />
            </svg>
            <span>{{ t('Exit') }}</span>
          </button>
        </div>
      </header>

      <!-- Middle area: video + sidebar side by side -->
      <div class="edu-middle" :class="middleClassObj" :style="middleStyle">
        <main class="edu-main">
          <!-- Video stage -->
          <section class="edu-stage">
            <div
              ref="eduVideoContainerRef"
              class="edu-video-container"
            >
              <!-- Main video stream -->
              <div class="main-video">
                <LiveView @empty-seat-click="handleApplyForSeat" />
              </div>

              <!-- Floating raise hand button -->
              <div
                v-show="!eduIsFullscreen"
                class="floating-raise-hand"
                @click="handleRaiseHand"
              >
                <div class="raise-hand-card" :class="raiseHandCardClass">
                  <svg class="raise-hand-icon" viewBox="0 0 24 24" fill="none">
                    <path d="M18 8.5V4a1.5 1.5 0 0 0-3 0v5" />
                    <path d="M15 8.5V3.5a1.5 1.5 0 0 0-3 0V8" />
                    <path d="M12 8V4a1.5 1.5 0 0 0-3 0v8" />
                    <path d="M9 12V7.5a1.5 1.5 0 0 0-3 0V16a6 6 0 0 0 6 6h1a6 6 0 0 0 6-6v-3.5a1.5 1.5 0 0 0-3 0" />
                  </svg>
                  <span class="raise-hand-text">{{ raiseHandText }}</span>
                </div>
              </div>

              <!-- Live ended overlay (dark backdrop to avoid visual flash) -->
              <div v-if="liveEndedOverlayVisible" class="live-ended-overlay">
                <div class="overlay-card ended-card">
                  <div class="ended-icon-shell">
                    <svg class="ended-icon" viewBox="0 0 24 24" fill="none">
                      <circle cx="12" cy="12" r="10" />
                      <line x1="15" y1="9" x2="9" y2="15" />
                      <line x1="9" y1="9" x2="15" y2="15" />
                    </svg>
                  </div>
                  <p class="ended-title">{{ t('The host is not currently live') }}</p>
                  <button class="primary-btn" @click="handleLeaveLive">
                    {{ t('Back to live list') }}
                  </button>
                </div>
              </div>
              <div v-if="kickedOutOverlayVisible" class="live-ended-overlay kicked-out-overlay">
                <div class="overlay-card ended-card">
                  <div class="ended-icon-shell warning">
                    <svg class="ended-icon" viewBox="0 0 24 24" fill="none">
                      <circle cx="12" cy="12" r="10" />
                      <path d="M12 7v6" />
                      <circle cx="12" cy="16.5" r="0.8" fill="currentColor" stroke="none" />
                    </svg>
                  </div>
                  <p class="ended-title">{{ t('Unable to watch live') }}</p>
                  <p class="ended-hint">{{ t('You have been removed from the live room and cannot watch the live stream') }}</p>
                  <button class="primary-btn" @click="handleLeaveLive">
                    {{ t('Back to home') }}
                  </button>
                </div>
              </div>
            </div>
          </section>
        </main>

        <Transition name="sidebar-slide">
          <aside v-show="!sidebarCollapsed" class="edu-sidebar">
            <div
              class="edu-sidebar-resizer"
              role="separator"
              aria-orientation="vertical"
              :aria-label="t('Resize sidebar')"
              @mousedown.prevent="handleSidebarResizeStart"
            />
            <EducationSidePanel :live-ended="liveEndedOverlayVisible" />
          </aside>
        </Transition>
      </div>

      <!-- Bottom toolbar: left = player controls, right = interaction + layout controls -->
      <div class="edu-bottom-toolbar">
        <!-- Left: player controls (volume, elapsed time) -->
        <div class="toolbar-group toolbar-player-controls">
          <div class="edu-tooltip-wrap">
            <button
              class="toolbar-icon-btn volume-btn"
              :class="{ disabled: isOnSeatCapabilityDisabled }"
              :disabled="isOnSeatCapabilityDisabled"
              @click="handleMuteToggle"
            >
              <svg v-if="isMuted" viewBox="0 0 24 24" fill="none">
                <path d="M4 10h4l5-4v12l-5-4H4z" />
                <path d="M17 9l4 4" />
                <path d="M21 9l-4 4" />
              </svg>
              <svg v-else viewBox="0 0 24 24" fill="none">
                <path d="M4 10h4l5-4v12l-5-4H4z" />
                <path d="M17 9a4 4 0 0 1 0 6" />
                <path d="M19.5 7a7 7 0 0 1 0 10" />
              </svg>
            </button>
            <div class="edu-btn-tooltip">{{ volumeTooltipText }}</div>
          </div>
          <div class="toolbar-volume-wrapper edu-tooltip-wrap" :class="{ 'tooltip-hidden': !isOnSeatCapabilityDisabled }">
            <div
              ref="volumeTrackRef"
              class="toolbar-volume-track"
              :class="{ disabled: isOnSeatCapabilityDisabled }"
              :style="{ '--toolbar-vol-percent': volumePercent }"
              @mousedown="onVolumeTrackMouseDown"
            >
              <div class="toolbar-volume-fill" />
              <div class="toolbar-volume-thumb" />
            </div>
            <div class="edu-btn-tooltip">{{ onSeatDisabledTooltipText }}</div>
          </div>
          <span class="toolbar-volume-text">{{ displayVolumePercent }}</span>
        </div>

        <!-- Right: layout controls -->
        <div class="toolbar-group toolbar-right-controls">
          <div class="edu-settings-wrapper">
            <div class="edu-tooltip-wrap" :class="{ 'tooltip-hidden': settingsPopupVisible && !isOnSeatCapabilityDisabled }">
              <button
                class="toolbar-icon-btn"
                :class="{ disabled: isOnSeatCapabilityDisabled }"
                :disabled="isOnSeatCapabilityDisabled"
                @click="openSettings"
              >
                <svg viewBox="0 0 24 24" fill="none">
                  <path d="M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 1 1.72v.51a2 2 0 0 1-1 1.74l-.15.09a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.39a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1-1-1.74v-.5a2 2 0 0 1 1-1.74l.15-.09a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z" />
                  <circle cx="12" cy="12" r="3" />
                </svg>
              </button>
              <div class="edu-btn-tooltip">{{ settingTooltipText }}</div>
            </div>
            <!-- Settings popup with resolution options -->
            <Transition name="edu-popup-fade">
              <div v-if="settingsPopupVisible" class="edu-settings-popup" @click.stop>
                <div class="settings-summary-label">
                  <div class="settings-summary-icon">
                    <svg viewBox="0 0 24 24" fill="none">
                      <path d="M4 7h10" />
                      <path d="M4 17h16" />
                      <path d="M14 7a2 2 0 1 0 0-4 2 2 0 0 0 0 4Z" />
                      <path d="M10 21a2 2 0 1 0 0-4 2 2 0 0 0 0 4Z" />
                    </svg>
                  </div>
                  <span>{{ t('Quality') }}</span>
                </div>
                <div class="settings-summary-value">
                  <button
                    v-if="canSwitchResolution && !resolutionListExpanded"
                    class="settings-current-option"
                    :class="{ disabled: isOnSeatCapabilityDisabled }"
                    :disabled="isOnSeatCapabilityDisabled"
                    @click="toggleResolutionList"
                  >
                    <span class="settings-current-text">{{ currentResolutionDisplayText }}</span>
                    <svg class="settings-option-arrow" viewBox="0 0 24 24" fill="none">
                      <path d="M7 10l5 5 5-5" />
                    </svg>
                  </button>
                  <div v-else-if="canSwitchResolution && resolutionListExpanded" class="settings-options-list">
                    <button
                      v-for="option in availableResolutions"
                      :key="option.value"
                      class="settings-option-item"
                      :class="{ active: option.value === sdkCurrentResolution?.value, disabled: isOnSeatCapabilityDisabled }"
                      :disabled="isOnSeatCapabilityDisabled"
                      @click="selectResolution(option)"
                    >
                      <span class="settings-current-text">{{ option.label }}</span>
                      <svg v-if="option.value === sdkCurrentResolution?.value" class="settings-option-check" viewBox="0 0 24 24" fill="none">
                        <path d="M5 12l5 5L20 7" />
                      </svg>
                    </button>
                  </div>
                  <div v-else class="settings-empty-state">
                    {{ currentResolutionDisplayText }}
                  </div>
                </div>
              </div>
            </Transition>
          </div>
          <div class="edu-tooltip-wrap">
            <button
              class="toolbar-icon-btn"
              :class="{ disabled: isSidebarToggleDisabled }"
              :disabled="isSidebarToggleDisabled"
              @click="toggleSidebar"
            >
              <svg v-if="sidebarCollapsed" viewBox="0 0 24 24" fill="none">
                <rect x="3" y="3" width="18" height="18" rx="2" ry="2" />
                <line x1="15" y1="3" x2="15" y2="21" />
                <polyline points="12 8 9 12 12 16" />
              </svg>
              <svg v-else viewBox="0 0 24 24" fill="none">
                <rect x="3" y="3" width="18" height="18" rx="2" ry="2" />
                <line x1="15" y1="3" x2="15" y2="21" />
                <polyline points="9 8 12 12 9 16" />
              </svg>
            </button>
            <div class="edu-btn-tooltip">{{ sidebarTooltipText }}</div>
          </div>
          <div class="edu-tooltip-wrap">
            <button class="toolbar-icon-btn" @click="handleNativeFullscreen">
              <svg v-if="!eduIsFullscreen" viewBox="0 0 24 24" fill="none">
                <path d="M8 4H4v4" />
                <path d="M16 4h4v4" />
                <path d="M8 20H4v-4" />
                <path d="M16 20h4v-4" />
              </svg>
              <svg v-else viewBox="0 0 24 24" fill="none">
                <path d="M4 8h4V4" />
                <path d="M16 4v4h4" />
                <path d="M4 16h4v4" />
                <path d="M20 16h-4v4" />
              </svg>
            </button>
            <div class="edu-btn-tooltip">{{ eduIsFullscreen ? t('Exit full screen') : t('Enter full screen') }}</div>
          </div>
        </div>
      </div>
    </section>

    <TUIDialog v-model:visible="exitLiveDialogVisible" :title="t('Exit Live')">
      {{ exitDialogContent }}
      <template #footer>
        <div class="dialog-actions">
          <button class="dialog-btn" @click="handleCancelExit">{{ t('Cancel') }}</button>
          <button v-if="isUserOnSeat" class="dialog-btn primary" @click="handleEndCoGuest">{{ t('End Co-guest') }}</button>
          <button class="dialog-btn primary" @click="handleExitLive">{{ t('Exit Live') }}</button>
        </div>
      </template>
    </TUIDialog>

    <LiveConnectionTypeDialog
      v-model="connectionTypeDialogVisible"
      v-model:type="requestConnectionType"
      @confirm="handleConnectionTypeConfirm"
      @cancel="handleConnectionTypeCancel"
    />
    <LiveDeviceSelectionDialog
      v-model="deviceSelectionDialogVisible"
      v-model:microphone-id="selectedMicrophoneId"
      v-model:camera-id="selectedCameraId"
      :type="requestConnectionType"
      :microphone-list="microphoneList"
      :camera-list="cameraList"
      @confirm="handleDeviceConfirm"
      @cancel="handleDeviceCancel"
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
  </div>
</template>

<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref, watch } from 'vue';
import TUIRoomEngine, { TUIRoomEvents } from '@tencentcloud/tuiroom-engine-js';
import { TUIDialog, TUIMessageBox, TUIToast, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { LiveListEvent, LiveView, UIKitModal, useLiveListState, useRoomEngine } from 'tuikit-atomicx-vue3';
import EducationSidePanel from './EducationSidePanel.vue';
import { useSeatApplication } from '../../../TUILiveKit/component/SeatApplication/useSeatApplication';
import LiveConnectionTypeDialog from '../../../TUILiveKit/component/LiveDialog/LiveConnectionTypeDialog.vue';
import LiveDeviceSelectionDialog from '../../../TUILiveKit/component/LiveDialog/LiveDeviceSelectionDialog.vue';
import { errorHandler } from '../../../TUILiveKit/utils/errorHandler';
import { initRoomEngineLanguage } from '../../../utils/utils';
import { usePlayerControlState } from '../composables/usePlayerControlState';

const { t } = useUIKit();
const props = defineProps<{ liveId: string }>();
const emit = defineEmits(['leaveLive', 'ready']);


const {
  isPlaying,
  isMuted,
  currentVolume,
  currentResolution: sdkCurrentResolution,
  resolutionList: sdkResolutionList,
  setVolume,
  mute,
  unmute,
  switchResolution: sdkSwitchResolution,
} = usePlayerControlState();

const { currentLive, joinLive, leaveLive, subscribeEvent, unsubscribeEvent } = useLiveListState();
const roomEngine = useRoomEngine();

const {
  handleApplyForSeat,
  openLeaveSeatDialog,
  isUserOnSeat,
  isApplyingSeat,
  confirmLeaveSeat,
  connectionTypeDialogVisible,
  deviceSelectionDialogVisible,
  cancelApplicationDialogVisible,
  leaveSeatDialogVisible,
  requestConnectionType,
  selectedMicrophoneId,
  selectedCameraId,
  microphoneList,
  cameraList,
  handleConnectionTypeConfirm,
  handleConnectionTypeCancel,
  handleDeviceConfirm,
  handleDeviceCancel,
  handleCancelApplicationOnSeat,
  handleCancelApplicationConfirm,
  handleCancelApplicationCancel,
  closeLeaveSeatDialog,
  initAutoSelectDevice,
  subscribeEvents,
  unsubscribeEvents,
} = useSeatApplication();

const liveContainerRef = ref<HTMLElement | null>(null);
const liveEndedOverlayVisible = ref(false);
const kickedOutOverlayVisible = ref(false);
const exitLiveDialogVisible = ref(false);
const readyEmitted = ref(false);

let autoPlayListenerBound = false;

const displayRoomTitle = computed(() => (currentLive.value?.liveName || '').trim() || t('Pan-Education Live Class'));

const exitDialogContent = computed(() => (isUserOnSeat.value
  ? t('LiveExitConfirmCoGuestTip')
  : t('Currently connected, do you need to "exit connection" or "end live broadcast"')));

watch(deviceSelectionDialogVisible, (visible) => {
  if (visible) {
    initAutoSelectDevice();
  }
});

watch(isPlaying, (playing) => {
  if (!playing) return;
  emitReadyOnce();
}, { immediate: true });

function emitReadyOnce() {
  if (readyEmitted.value) return;
  readyEmitted.value = true;
  emit('ready');
}

function bindAutoPlayFailedListener() {
  if (!roomEngine.instance || autoPlayListenerBound) return;
  roomEngine.instance.on(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
  autoPlayListenerBound = true;
}

function handleAutoPlayFailed() {
  emitReadyOnce();
}

async function handleJoinLive(): Promise<boolean> {
  if (!props.liveId || !props.liveId.trim()) {
    TUIMessageBox.alert({
      title: t('Unable to watch live'),
      content: t('LiveId is empty'),
      confirmText: t('Back to home'),
      showClose: false,
      modal: false,
      callback: () => emit('leaveLive'),
    });
    return false;
  }

  try {
    await joinLive({ liveId: props.liveId });
    return true;
  } catch (error: any) {
    const errorInfo = errorHandler.parseError(error);
    UIKitModal.openModal({
      id: errorInfo.code,
      title: t('Failed to join live room'),
      content: t(errorInfo.message),
      type: 'error',
    });
    emit('leaveLive');
    return false;
  }
}

// ====== Custom toolbar state ======
const eduVideoContainerRef = ref<HTMLElement | null>(null);
const volumeTrackRef = ref<HTMLElement | null>(null);
const eduIsFullscreen = ref(false);
const sidebarCollapsed = ref(false);
const isPortraitStream = ref(false);
const settingsPopupVisible = ref(false);
const resolutionListExpanded = ref(false);
const sidebarWidth = ref(360);
const hasManualSidebarWidth = ref(false);

const SIDEBAR_MIN_WIDTH = 300;
const SIDEBAR_MAX_WIDTH = 560;
const SIDEBAR_DEFAULT_WIDTH = 360;
const SIDEBAR_PORTRAIT_WIDTH = 480;

// Resolution - derived from SDK state
const availableResolutions = computed(() => sdkResolutionList.value);
const canSwitchResolution = computed(() => availableResolutions.value.length > 1);
const currentResolutionDisplayText = computed(() => sdkCurrentResolution.value?.label || t('No resolution available'));
const preSeatVolume = ref(currentVolume.value);
const raiseHandText = computed(() => {
  if (isApplyingSeat.value) {
    return t('Cancel Raise Hand');
  }
  return isUserOnSeat.value ? t('Leave Stage') : t('Raise Hand');
});
const onSeatDisabledTooltipText = '上台后无法使用此能力';
const isOnSeatCapabilityDisabled = computed(() => isUserOnSeat.value);
const volumeTooltipText = computed(() => (
  isOnSeatCapabilityDisabled.value
    ? onSeatDisabledTooltipText
    : (isMuted.value ? t('Turn on sound') : t('Mute sound'))
));
const settingTooltipText = computed(() => (
  isOnSeatCapabilityDisabled.value ? onSeatDisabledTooltipText : t('Setting')
));
const raiseHandCardClass = computed(() => ({
  active: isUserOnSeat.value,
  pending: isApplyingSeat.value,
}));
const isSidebarToggleDisabled = computed(() => eduIsFullscreen.value);
const sidebarTooltipText = computed(() => {
  if (isSidebarToggleDisabled.value) {
    return t('Sidebar unavailable in fullscreen');
  }
  return sidebarCollapsed.value ? t('Show Sidebar') : t('Hide Sidebar');
});

const toggleSidebar = () => {
  if (isSidebarToggleDisabled.value) {
    return;
  }
  sidebarCollapsed.value = !sidebarCollapsed.value;
};

const middleClassObj = computed(() => ({
  'sidebar-collapsed': sidebarCollapsed.value,
  'portrait-stream': isPortraitStream.value && !sidebarCollapsed.value,
}));
const middleStyle = computed(() => ({
  '--edu-sidebar-width': `${sidebarCollapsed.value ? 0 : sidebarWidth.value}px`,
}));

function clampSidebarWidth(width: number) {
  return Math.max(SIDEBAR_MIN_WIDTH, Math.min(SIDEBAR_MAX_WIDTH, width));
}

let sidebarResizeStartX = 0;
let sidebarResizeStartWidth = SIDEBAR_DEFAULT_WIDTH;
let isSidebarResizing = false;

function handleSidebarResizeMove(event: MouseEvent) {
  if (!isSidebarResizing || sidebarCollapsed.value) return;
  const deltaX = sidebarResizeStartX - event.clientX;
  const nextWidth = clampSidebarWidth(sidebarResizeStartWidth + deltaX);
  sidebarWidth.value = nextWidth;
  hasManualSidebarWidth.value = true;
}

function stopSidebarResize() {
  if (!isSidebarResizing) return;
  isSidebarResizing = false;
  document.removeEventListener('mousemove', handleSidebarResizeMove);
  document.removeEventListener('mouseup', stopSidebarResize);
  document.body.style.cursor = '';
  document.body.style.userSelect = '';
}

function handleSidebarResizeStart(event: MouseEvent) {
  if (sidebarCollapsed.value) return;
  isSidebarResizing = true;
  sidebarResizeStartX = event.clientX;
  sidebarResizeStartWidth = sidebarWidth.value;
  document.addEventListener('mousemove', handleSidebarResizeMove);
  document.addEventListener('mouseup', stopSidebarResize);
  document.body.style.cursor = 'col-resize';
  document.body.style.userSelect = 'none';
}

// Computed
const volumePercent = computed(() => currentVolume.value);
const displayVolumePercent = computed(() => `${Math.round(volumePercent.value)}`);

// ====== Volume slider drag logic ======
const applyVolume = (percent: number) => {
  const clamped = Math.max(0, Math.min(100, percent));
  setVolume(clamped);
  if (clamped === 0) {
    mute();
  } else if (isMuted.value) {
    unmute();
  }
};

const onVolumeTrackMouseDown = (e: MouseEvent) => {
  if (isOnSeatCapabilityDisabled.value) return;
  const track = volumeTrackRef.value;
  if (!track) return;
  const updateFromEvent = (ev: MouseEvent) => {
    const rect = track.getBoundingClientRect();
    const percent = ((ev.clientX - rect.left) / rect.width) * 100;
    applyVolume(percent);
  };
  updateFromEvent(e);
  const onMouseMove = (ev: MouseEvent) => updateFromEvent(ev);
  const onMouseUp = () => {
    document.removeEventListener('mousemove', onMouseMove);
    document.removeEventListener('mouseup', onMouseUp);
  };
  document.addEventListener('mousemove', onMouseMove);
  document.addEventListener('mouseup', onMouseUp);
};

// ====== Control handlers ======
const handleMuteToggle = () => {
  if (isOnSeatCapabilityDisabled.value) {
    return;
  }
  if (isMuted.value) {
    unmute();
  } else {
    mute();
  }
};

const openSettings = () => {
  if (isOnSeatCapabilityDisabled.value) {
    settingsPopupVisible.value = false;
    resolutionListExpanded.value = false;
    return;
  }
  if (!settingsPopupVisible.value) {
    resolutionListExpanded.value = false;
  }
  settingsPopupVisible.value = !settingsPopupVisible.value;
};

const toggleResolutionList = () => {
  if (isOnSeatCapabilityDisabled.value) return;
  if (!canSwitchResolution.value) return;
  resolutionListExpanded.value = !resolutionListExpanded.value;
};

/**
 * Close settings popup when clicking outside the wrapper.
 */
const handleSettingsOutsideClick = (e: MouseEvent) => {
  const wrapper = document.querySelector('.edu-settings-wrapper');
  if (wrapper && !wrapper.contains(e.target as Node)) {
    settingsPopupVisible.value = false;
    resolutionListExpanded.value = false;
  }
};

/**
 * Select a resolution option via SDK API and close popup.
 */
const selectResolution = (opt: { label: string; value: number }) => {
  if (isOnSeatCapabilityDisabled.value) return;
  sdkSwitchResolution(opt);
  resolutionListExpanded.value = false;
  settingsPopupVisible.value = false;
};

// ====== Native fullscreen (on video container element) ======
const handleNativeFullscreen = async () => {
  const el = liveContainerRef.value;
  if (!el) return;
  if (eduIsFullscreen.value) {
    try {
      await document.exitFullscreen();
    } catch { /* ignore */ }
  } else {
    try {
      await el.requestFullscreen();
    } catch { /* ignore */ }
  }
};

const onFullscreenChange = () => {
  eduIsFullscreen.value = !!document.fullscreenElement;
  if (eduIsFullscreen.value) {
    sidebarCollapsed.value = true;
  }
};

watch(isPortraitStream, (portrait) => {
  if (hasManualSidebarWidth.value) return;
  sidebarWidth.value = clampSidebarWidth(portrait ? SIDEBAR_PORTRAIT_WIDTH : SIDEBAR_DEFAULT_WIDTH);
}, { immediate: true });

watch([currentVolume, isUserOnSeat], ([volume, onSeat]) => {
  if (!onSeat) {
    preSeatVolume.value = volume;
  }
}, { immediate: true });

watch(isUserOnSeat, (onSeat, wasOnSeat) => {
  if (!onSeat || wasOnSeat) return;
  const targetVolume = Math.max(0, Math.min(100, preSeatVolume.value));
  if (Math.round(currentVolume.value) !== Math.round(targetVolume)) {
    applyVolume(targetVolume);
  }
});

watch(isOnSeatCapabilityDisabled, (disabled) => {
  if (!disabled) return;
  settingsPopupVisible.value = false;
  resolutionListExpanded.value = false;
});

function handleRaiseHand() {
  if (isUserOnSeat.value) {
    openLeaveSeatDialog();
    return;
  }
  if (isApplyingSeat.value) {
    handleCancelApplicationOnSeat();
    return;
  }
  handleApplyForSeat();
}

function handleLiveEnded() {
  liveEndedOverlayVisible.value = true;
  kickedOutOverlayVisible.value = false;
  emitReadyOnce();
}

function handleKickedOutOfLive() {
  kickedOutOverlayVisible.value = true;
  liveEndedOverlayVisible.value = false;
  emitReadyOnce();
}

function handleLeaveLive() {
  if (isUserOnSeat.value) {
    exitLiveDialogVisible.value = true;
    return;
  }
  emit('leaveLive');
}

function handleCancelExit() {
  exitLiveDialogVisible.value = false;
}

function handleExitLive() {
  exitLiveDialogVisible.value = false;
  emit('leaveLive');
}

async function handleEndCoGuest() {
  exitLiveDialogVisible.value = false;
  try {
    await confirmLeaveSeat();
  } catch {
    TUIToast.error({ message: t('Failed to leave seat') });
  }
}

// ====== Portrait stream detection via ResizeObserver ======
let videoResizeObserver: ResizeObserver | null = null;

function startPortraitDetection() {
  const el = eduVideoContainerRef.value;
  if (!el) return;
  videoResizeObserver = new ResizeObserver((entries) => {
    for (const entry of entries) {
      const { width, height } = entry.contentRect;
      // Detect portrait-ish aspect ratio (height > width * 1.1 means vertical video)
      isPortraitStream.value = height > width * 1.1;
    }
  });
  videoResizeObserver.observe(el);
}

function stopPortraitDetection() {
  videoResizeObserver?.disconnect();
  videoResizeObserver = null;
}

onMounted(async () => {
  startPortraitDetection();
  bindAutoPlayFailedListener();
  if (!autoPlayListenerBound) {
    TUIRoomEngine.once('ready', bindAutoPlayFailedListener);
  }
  subscribeEvent(LiveListEvent.onLiveEnded, handleLiveEnded);
  subscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  subscribeEvents();
  document.addEventListener('fullscreenchange', onFullscreenChange);
  document.addEventListener('click', handleSettingsOutsideClick);

  await initRoomEngineLanguage();
  const joined = await handleJoinLive();
  if (!joined) {
    emitReadyOnce();
  }
});

onUnmounted(async () => {
  stopSidebarResize();
  stopPortraitDetection();
  unsubscribeEvent(LiveListEvent.onLiveEnded, handleLiveEnded);
  unsubscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  unsubscribeEvents();
  document.removeEventListener('fullscreenchange', onFullscreenChange);
  document.removeEventListener('click', handleSettingsOutsideClick);

  if (currentLive.value?.liveId) {
    await leaveLive();
  }

  if (autoPlayListenerBound) {
    roomEngine.instance?.off(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
    autoPlayListenerBound = false;
  }
});
</script>

<style scoped lang="scss">
.live-player-education-pc {
  width: 100%;
  height: 100%;
  background: var(--edu-bg-page);
  color: var(--edu-text-primary);
}

// === Three-row layout: header / middle / footer ===
.edu-layout {
  display: grid;
  grid-template-rows: auto minmax(0, 1fr) auto;
  width: 100%;
  height: 100%;
  box-sizing: border-box;
}

.edu-middle {
  display: grid;
  grid-template-columns: minmax(0, 1fr) var(--edu-sidebar-width, 360px);
  min-height: 0;
  transition: grid-template-columns 280ms ease;
}

.edu-main {
  display: flex;
  flex-direction: column;
  min-width: 0;
  min-height: 0;
}

// === Header bar ===
.edu-header-bar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 16px;
  padding: 8px 20px;
  background: var(--edu-bg-surface);
  border-bottom: 1px solid var(--edu-border);
}

.header-left {
  display: flex;
  align-items: center;
  gap: 12px;
  min-width: 0;
}

.edu-scene-brand {
  display: inline-flex;
  align-items: center;
  gap: 10px;
  padding-right: 14px;
  border-right: 1px solid var(--edu-border);
  flex-shrink: 0;
}

.edu-scene-brand-logo {
  display: block;
  width: 28px;
  height: 28px;
  object-fit: contain;
}

.edu-scene-brand-tag {
  display: inline-flex;
  align-items: center;
  height: 32px;
  padding: 0 14px;
  border-radius: 999px;
  background: color-mix(in srgb, var(--edu-primary) 12%, var(--edu-bg-surface));
  color: var(--edu-primary);
  font-size: 14px;
  font-weight: 600;
  letter-spacing: 0.02em;
  line-height: 1;
}

.course-title {
  margin: 0;
  font-size: var(--edu-font-size-title);
  font-weight: 600;
  line-height: 1.3;
  white-space: nowrap;
  text-overflow: ellipsis;
  overflow: hidden;
  color: var(--edu-text-primary);
}

.header-right {
  display: flex;
  align-items: center;
  gap: 8px;
  flex-shrink: 0;
}

.header-exit-btn {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  height: 32px;
  padding: 0 12px;
  border: 1px solid var(--edu-border);
  border-radius: 999px;
  background: var(--edu-bg-surface);
  color: var(--edu-text-secondary);
  font-size: 13px;
  font-weight: 500;
  cursor: pointer;
  transition: all 180ms ease;

  &:hover {
    color: var(--edu-text-primary);
    background: var(--edu-bg-hover);
    border-color: var(--edu-border-strong);
  }
}

.header-exit-icon {
  width: 14px;
  height: 14px;
  stroke: currentColor;
  stroke-width: 1.8;
  stroke-linecap: round;
  stroke-linejoin: round;
  flex-shrink: 0;
}

// === Separate status capsules ===
.status-capsule {
  display: flex;
  align-items: center;
  gap: 6px;
  height: 32px;
  padding: 0 14px;
  border-radius: 999px;
  background: var(--edu-bg-muted);
  font-size: 13px;
  font-weight: 500;
  color: var(--edu-text-secondary);
  font-variant-numeric: tabular-nums;
  white-space: nowrap;
}

.live-capsule {
  background: color-mix(in srgb, var(--edu-danger) 10%, transparent);
  color: var(--edu-danger);

  .live-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: var(--edu-danger);
    animation: livePulse 1.4s ease-in-out infinite;
    flex-shrink: 0;
  }

  .live-text {
    font-weight: 600;
  }
}

// === Video stage ===
.edu-stage {
  flex: 1;
  min-height: 0;
  position: relative;
}

.edu-video-container {
  position: relative;
  width: 100%;
  height: 100%;
  background: var(--edu-pip-bg);
  overflow: hidden;

  :deep(.playback-controls) {
    position: fixed !important;
    top: -9999px !important;
    left: -9999px !important;
    opacity: 0 !important;
    pointer-events: none !important;
    width: 1px !important;
    height: 1px !important;
    overflow: hidden !important;
    z-index: -1 !important;
  }
}

.main-video {
  position: absolute;
  inset: 0;

  // Ensure LiveView fills the container properly (same approach as business style)
  :deep(> *:first-child) {
    position: absolute !important;
    inset: 0 !important;
    width: 100% !important;
    height: 100% !important;
  }
}

// === Floating raise hand button ===
.floating-raise-hand {
  position: absolute;
  right: 10px;
  top: 50%;
  transform: translateY(-50%);
  z-index: 15;
  cursor: pointer;
  user-select: none;
  opacity: 0.68;
  transition: opacity 180ms ease;

  &:hover {
    opacity: 0.88;
  }

  &:hover .raise-hand-card {
    width: 42px;
    height: 84px;
    border-radius: 22px;
    padding: 9px 7px;
    gap: 7px;
    background: var(--edu-fab-bg-hover);
    box-shadow: var(--edu-fab-shadow-hover);
    transform: translateY(-1px);
  }

  .raise-hand-card.pending {
    width: 42px;
    height: 84px;
    border-radius: 22px;
    padding: 9px 7px;
    gap: 7px;
    transform: translateY(-1px);
  }

  &:hover .raise-hand-text,
  .raise-hand-card.pending .raise-hand-text {
    max-height: 22px;
    opacity: 1;
    transform: translateY(0);
  }
}

.raise-hand-card {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 0;
  width: 34px;
  height: 60px;
  padding: 9px 7px;
  border-radius: 18px;
  border: 1px solid var(--edu-fab-border);
  background: var(--edu-fab-bg);
  box-shadow: var(--edu-fab-shadow);
  overflow: hidden;
  transition: width 280ms cubic-bezier(0.22, 1, 0.36, 1),
    height 280ms cubic-bezier(0.22, 1, 0.36, 1),
    border-radius 280ms cubic-bezier(0.22, 1, 0.36, 1),
    padding 280ms cubic-bezier(0.22, 1, 0.36, 1),
    gap 280ms cubic-bezier(0.22, 1, 0.36, 1),
    background 220ms ease,
    box-shadow 220ms ease,
    transform 280ms cubic-bezier(0.22, 1, 0.36, 1);

  &:active {
    transform: scale(0.98);
  }

  &.active {
    background: color-mix(in srgb, var(--edu-warning) 15%, var(--edu-fab-bg));
    border: 1px solid var(--edu-warning);

    .raise-hand-icon {
      stroke: var(--edu-warning);
    }

    .raise-hand-text {
      color: var(--edu-warning);
    }
  }

  &.pending {
    background: color-mix(in srgb, var(--edu-primary) 14%, var(--edu-fab-bg));
    border-color: color-mix(in srgb, var(--edu-primary) 58%, var(--edu-fab-border));
    box-shadow: 0 8px 24px color-mix(in srgb, var(--edu-primary) 24%, transparent);

    .raise-hand-icon {
      stroke: var(--edu-primary);
    }

    .raise-hand-text {
      color: var(--edu-primary);
    }
  }
}

.raise-hand-icon {
  width: 18px;
  height: 18px;
  stroke: var(--edu-fab-icon);
  stroke-width: 1.8;
  stroke-linecap: round;
  stroke-linejoin: round;
  fill: none;
  flex-shrink: 0;
}

.raise-hand-text {
  font-size: 10px;
  color: var(--edu-fab-text);
  font-weight: 500;
  text-align: center;
  line-height: 1.05;
  white-space: normal;
  overflow: hidden;
  max-height: 0;
  opacity: 0;
  transform: translateY(6px);
  transition: max-height 280ms cubic-bezier(0.22, 1, 0.36, 1),
    opacity 220ms ease,
    transform 280ms cubic-bezier(0.22, 1, 0.36, 1);
}

// === Bottom toolbar (two groups: left + right) ===
.edu-bottom-toolbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  height: 48px;
  padding: 0 12px;
  background: var(--edu-toolbar-bg);
  border-top: 1px solid var(--edu-toolbar-border);
  box-sizing: border-box;
}

.toolbar-group {
  display: flex;
  align-items: center;
  gap: 4px;
}

.toolbar-divider {
  width: 1px;
  height: 20px;
  margin: 0 4px;
  background: var(--edu-border);
  flex-shrink: 0;
}

.toolbar-icon-btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 36px;
  height: 36px;
  border: none;
  border-radius: var(--edu-radius-sm);
  background: transparent;
  color: var(--edu-toolbar-icon-color);
  cursor: pointer;
  padding: 0;
  flex-shrink: 0;
  transition: color 150ms ease, background 150ms ease;

  svg {
    width: 20px;
    height: 20px;
    stroke: currentColor;
    stroke-width: 2;
    stroke-linecap: round;
    stroke-linejoin: round;
    fill: none;
  }

  // Volume icon needs fill for speaker body
  &.volume-btn svg {
    width: 24px;
    height: 24px;

    path:first-child {
      fill: currentColor;
      stroke: none;
    }
  }

  &:hover {
    color: var(--edu-toolbar-icon-hover);
    background: var(--edu-bg-hover);
  }

  &:disabled,
  &.disabled {
    color: var(--edu-text-tertiary);
    background: transparent;
    cursor: not-allowed;
    opacity: 0.48;
  }
}

.volume-btn {
  width: 40px;
  height: 40px;
  border-radius: 10px;
  background: color-mix(in srgb, var(--edu-bg-hover) 72%, transparent);

  &:hover {
    background: color-mix(in srgb, var(--edu-primary) 10%, var(--edu-bg-hover));
  }
}

// Volume slider
.toolbar-volume-wrapper {
  display: flex;
  align-items: center;
  width: 104px;
  padding: 0 4px 0 2px;

  &.edu-tooltip-wrap {
    justify-content: flex-start;
  }
}

.toolbar-volume-track {
  position: relative;
  width: 100%;
  height: 4px;
  background: var(--edu-toolbar-slider-track);
  border-radius: 9999px;
  cursor: pointer;
  --toolbar-thumb-size: 12px;
  --toolbar-thumb-half: 6px;

  &.disabled {
    cursor: not-allowed;
    opacity: 0.42;
  }
}

.toolbar-volume-fill {
  position: relative;
  z-index: 1;
  height: 100%;
  width: calc(var(--toolbar-vol-percent, 0) * 1%);
  background: var(--edu-toolbar-slider-fill);
  border-radius: 9999px;
  transition: width 50ms linear;
}

.toolbar-volume-thumb {
  position: absolute;
  z-index: 3;
  top: 50%;
  transform: translate(-50%, -50%);
  width: var(--toolbar-thumb-size);
  height: var(--toolbar-thumb-size);
  border-radius: 50%;
  background: var(--edu-toolbar-slider-thumb);
  box-shadow: 0 0 4px rgba(0, 0, 0, 0.15);
  cursor: grab;
  left: clamp(
    var(--toolbar-thumb-half),
    calc(var(--toolbar-vol-percent, 0) * 1%),
    calc(100% - var(--toolbar-thumb-half))
  );
  transition: left 50ms linear;

  .toolbar-volume-track.disabled & {
    cursor: not-allowed;
  }

  &:active {
    cursor: grabbing;
    transform: translate(-50%, -50%) scale(1.15);
  }
}

.toolbar-volume-text {
  font-size: 13px;
  font-weight: 500;
  color: var(--edu-toolbar-text-color);
  font-variant-numeric: tabular-nums;
  min-width: 30px;
  text-align: center;
  user-select: none;
}

// === Sidebar ===
.edu-sidebar {
  position: relative;
  min-width: 0;
  min-height: 0;
  border-left: 1px solid var(--edu-border);
  overflow: hidden;
}

.edu-sidebar-resizer {
  position: absolute;
  left: 0;
  top: 0;
  bottom: 0;
  width: 8px;
  transform: translateX(-50%);
  cursor: col-resize;
  z-index: 5;
  background: transparent;

  &::before {
    content: '';
    position: absolute;
    left: 50%;
    top: 0;
    bottom: 0;
    width: 2px;
    transform: translateX(-50%);
    background: color-mix(in srgb, var(--edu-primary) 10%, transparent);
    transition: background 180ms ease;
  }

  &:hover::before {
    background: color-mix(in srgb, var(--edu-primary) 54%, transparent);
  }
}

// Sidebar slide transition
.sidebar-slide-enter-active,
.sidebar-slide-leave-active {
  transition: opacity 200ms ease, transform 200ms ease;
}

.sidebar-slide-enter-from,
.sidebar-slide-leave-to {
  opacity: 0;
  transform: translateX(20px);
}

// === Overlays ===
.live-ended-overlay {
  position: absolute;
  inset: 0;
  z-index: 20;
  background: var(--edu-ended-overlay-bg);
  display: grid;
  place-items: center;
}

.overlay-card {
  width: min(360px, calc(100% - 32px));
  border: 1px solid var(--edu-ended-card-border, var(--edu-border));
  border-radius: var(--edu-radius-lg);
  background: var(--edu-ended-card-bg, var(--edu-bg-surface));
  box-shadow: var(--edu-shadow-card);
  padding: 20px;
  text-align: center;

  p {
    margin: 0;
    color: var(--edu-text-primary);
    font-size: var(--edu-font-size-body);
  }
}

.ended-card {
  padding: 32px 24px;
}

.ended-icon-shell {
  width: 64px;
  height: 64px;
  margin: 0 auto 14px;
  border-radius: 50%;
  display: grid;
  place-items: center;
  background: var(--edu-ended-icon-bg);
  color: var(--edu-ended-icon-color);

  &.warning {
    background: color-mix(in srgb, var(--edu-warning) 16%, var(--edu-bg-surface));
    color: var(--edu-warning);
  }
}

.ended-icon {
  width: 48px;
  height: 48px;
  stroke: currentColor;
  stroke-width: 1.5;
  stroke-linecap: round;
  stroke-linejoin: round;
}

.ended-title {
  font-weight: 600;
  margin-bottom: 6px !important;
}

.ended-hint {
  font-size: var(--edu-font-size-small) !important;
  color: var(--edu-text-tertiary) !important;
  margin-bottom: 4px !important;
  line-height: 1.5;
}

.primary-btn {
  margin-top: 14px;
  height: 36px;
  border-radius: 20px;
  border: none;
  background: var(--edu-primary);
  color: var(--edu-text-on-primary);
  padding: 0 20px;
  cursor: pointer;
  font-size: var(--edu-font-size-body);
  font-weight: 600;

  &:hover {
    background: var(--edu-primary-hover);
  }
}

// === Dialog actions ===
.dialog-actions {
  display: flex;
  justify-content: flex-end;
  gap: 8px;
}

.dialog-btn {
  height: 34px;
  border-radius: 20px;
  border: 1px solid var(--edu-border);
  background: var(--edu-bg-surface);
  color: var(--edu-text-secondary);
  padding: 0 14px;
  cursor: pointer;
  font-size: var(--edu-font-size-caption);

  &.primary {
    border-color: var(--edu-primary);
    background: var(--edu-primary);
    color: var(--edu-text-on-primary);
  }

  &.primary:hover {
    background: var(--edu-primary-hover);
    border-color: var(--edu-primary-hover);
  }
}

/* Ensure raise-hand related dialogs always use education green accents */
:deep(.request-connection-dialog),
:deep(.device-selection-dialog) {
  --text-color-link-hover: var(--edu-primary);
}

:deep(.request-connection-dialog .option-card.active) {
  border-color: var(--edu-primary) !important;
}

:deep(.request-connection-dialog .option-card.active .option-icon) {
  color: var(--edu-primary) !important;
}

// === Transitions ===
.edu-overlay-fade-enter-active,
.edu-overlay-fade-leave-active {
  transition: opacity 220ms ease;
}

.edu-overlay-fade-enter-from,
.edu-overlay-fade-leave-to {
  opacity: 0;
}

@keyframes livePulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.4; }
}

// === Responsive ===
@media (max-width: 1280px) {
  .edu-middle:not(.sidebar-collapsed) {
    grid-template-columns: 1fr;
    grid-template-rows: minmax(0, 1fr) 340px;

    &.portrait-stream {
      grid-template-columns: 1fr;
    }
  }

  .edu-sidebar {
    border-left: none;
    border-top: 1px solid var(--edu-border);
  }

  .edu-sidebar-resizer {
    display: none;
  }
}

@media (max-width: 920px) {
  .edu-header-bar {
    flex-direction: column;
    align-items: flex-start;
    gap: 6px;
  }
}

// === Tooltip wrapper ===
.edu-tooltip-wrap {
  position: relative;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex-shrink: 0;

  .edu-btn-tooltip {
    position: absolute;
    bottom: calc(100% + 8px);
    left: 50%;
    transform: translateX(-50%) translateY(4px);
    opacity: 0;
    pointer-events: none;
    white-space: nowrap;
    font-size: 12px;
    line-height: 1;
    color: var(--edu-tooltip-text);
    background: var(--edu-tooltip-bg);
    border: 1px solid var(--edu-tooltip-border);
    border-radius: 8px;
    padding: 8px 10px;
    box-shadow: var(--edu-tooltip-shadow);
    transition: opacity 180ms ease, transform 180ms ease;
    z-index: 80;
  }

  &:hover .edu-btn-tooltip {
    opacity: 1;
    transform: translateX(-50%) translateY(0);
  }

  &.tooltip-hidden .edu-btn-tooltip {
    opacity: 0 !important;
    transform: translateX(-50%) translateY(4px) !important;
  }
}

// === Settings wrapper ===
.edu-settings-wrapper {
  position: relative;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}

// === Settings popup ===
.edu-settings-popup {
  position: absolute;
  right: 0;
  bottom: calc(100% + 12px);
  display: grid;
  grid-template-columns: auto minmax(144px, 208px);
  align-items: stretch;
  min-width: 248px;
  max-width: 320px;
  min-height: 60px;
  border-radius: 14px;
  overflow: hidden;
  background: var(--edu-settings-popup-bg);
  box-shadow: 0 12px 28px rgba(0, 0, 0, 0.24);
  border: 1px solid var(--edu-settings-popup-border);
  z-index: 120;
  backdrop-filter: blur(10px);
}

.settings-summary-label {
  display: flex;
  align-items: center;
  gap: 10px;
  min-height: 60px;
  padding: 0 14px;
  background: var(--edu-settings-popup-panel-bg);
  border-right: 1px solid var(--edu-settings-popup-border);
  color: var(--edu-settings-popup-text);
  font-size: 15px;
  font-weight: 500;
  white-space: nowrap;
}

.settings-summary-icon {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 18px;
  height: 18px;

  svg {
    width: 18px;
    height: 18px;
    stroke: currentColor;
    stroke-width: 1.8;
    stroke-linecap: round;
    stroke-linejoin: round;
  }
}

.settings-summary-value {
  display: flex;
  flex-direction: column;
  align-items: stretch;
  min-width: 144px;
  min-height: 60px;
  background: transparent;
}

.settings-current-option,
.settings-option-item {
  appearance: none;
  border: none;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  min-height: 44px;
  padding: 0 14px;
  background: transparent;
  cursor: pointer;
  text-align: right;
  color: var(--edu-settings-popup-subtle-text);
  font-size: 14px;
  font-weight: 500;
  transition: background 150ms ease, color 150ms ease;

  &:hover {
    background: var(--edu-settings-popup-hover);
    color: var(--edu-settings-popup-text);
  }

  &.active {
    background: var(--edu-settings-popup-hover);
    color: var(--edu-settings-popup-text);
  }

  &:disabled,
  &.disabled {
    cursor: not-allowed;
    opacity: 0.52;

    &:hover {
      background: transparent;
      color: var(--edu-settings-popup-subtle-text);
    }
  }
}

.settings-current-option {
  flex: 1;
  min-height: 60px;
  color: var(--edu-settings-popup-text);

  &.expanded {
    background: var(--edu-settings-popup-hover);
  }
}

.settings-options-list {
  display: flex;
  flex-direction: column;
  border-top: 1px solid var(--edu-settings-popup-border);
}

.settings-current-text {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  line-height: 1.2;
  flex: 1;
}

.settings-option-arrow,
.settings-option-check {
  width: 16px;
  height: 16px;
  stroke: currentColor;
  stroke-width: 1.8;
  stroke-linecap: round;
  stroke-linejoin: round;
  flex-shrink: 0;
}

.settings-option-arrow {
  transition: transform 150ms ease;
}

.settings-current-option.expanded .settings-option-arrow {
  transform: rotate(180deg);
}

.settings-empty-state {
  min-height: 60px;
  display: flex;
  align-items: center;
  padding: 0 14px;
  color: var(--edu-settings-popup-subtle-text);
  font-size: 13px;
}

// === Settings popup transition ===
.edu-popup-fade-enter-active,
.edu-popup-fade-leave-active {
  transition: opacity 220ms cubic-bezier(0.22, 1, 0.36, 1);
  transform-origin: bottom right;
  will-change: opacity, transform, filter;
}

.edu-popup-fade-enter-from,
.edu-popup-fade-leave-to {
  opacity: 0;
  filter: blur(6px);
}

.edu-popup-fade-enter-from.edu-settings-popup,
.edu-popup-fade-leave-to.edu-settings-popup {
  transform: translateY(10px) scale(0.94);
}

.edu-popup-fade-enter-active.edu-settings-popup,
.edu-popup-fade-leave-active.edu-settings-popup {
  transition:
    opacity 220ms cubic-bezier(0.22, 1, 0.36, 1),
    transform 260ms cubic-bezier(0.22, 1, 0.36, 1),
    filter 220ms ease;
}
</style>
