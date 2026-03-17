<template>
  <div
    id="liveContainer"
    ref="liveContainerRef"
    class="live-player-business-pc"
    :class="{ 'panel-collapsed': sidePanelCollapsed }"
  >
    <!-- Left: Immersive video stage -->
    <div class="biz-left-column">
      <div
        ref="bizVideoCardRef"
        class="biz-video-card"
        :class="{ 'controls-visible': showBizControls || (!isPlaying && isInLive && !liveEndedOverlayVisible) }"
        @mouseenter="showBizControls = true; cancelBizControlsHideTimer()"
        @mouseleave="startBizControlsHideTimer()"
      >
        <LiveView @empty-seat-click="handleApplyForSeat">
          <template #center-overlay>
            <Transition name="biz-manual-refresh-fade">
              <div v-if="isManualRefreshing" class="biz-manual-refresh-overlay" aria-hidden="true">
                <div class="biz-manual-refresh-spinner">
                  <svg class="biz-manual-refresh-orbit" viewBox="0 0 128 128">
                    <circle class="orbit-track" cx="64" cy="64" r="50" />
                    <g class="orbit-segments">
                      <circle class="orbit-segment" cx="64" cy="64" r="50" />
                      <circle class="orbit-segment orbit-segment-alt" cx="64" cy="64" r="50" />
                    </g>
                  </svg>
                  <img class="biz-manual-refresh-logo" src="../../assets/imgs/logo.svg" alt="logo" />
                </div>
              </div>
            </Transition>
          </template>
        </LiveView>
        <div class="biz-video-top-gradient" />
        <div class="biz-video-top-overlay">
          <div class="stream-top-unified">
            <div class="stream-meta-glass">
              <div class="stream-host-avatar">
                <Avatar
                  :src="currentLive?.liveOwner.avatarUrl"
                  :size="44"
                  class="stream-host-avatar-img"
                />
              </div>
              <div class="stream-meta-text">
                <div class="stream-title-row">
                  <div class="stream-title">
                    {{ displayRoomTitle }}
                  </div>
                  <span class="stream-live-badge">
                    <span class="stream-live-badge-dot" />
                    LIVE
                  </span>
                </div>
                <div class="stream-subtitle">
                  <span class="stream-subtitle-host">{{ displayHostName }}</span>
                  <span class="stream-subtitle-sep">·</span>
                  <span class="stream-subtitle-room">Room {{ currentLive?.liveId || '--' }}</span>
                </div>
              </div>
            </div>
            <div class="stream-actions-glass top-action-segmented">
              <button class="top-seg-btn top-seg-btn-primary" :title="t('Copy Link')" @click="handleCopyLink">
                <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                  <path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71" />
                  <path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71" />
                </svg>
                <span>{{ t('Copy Link') }}</span>
              </button>
              <button class="top-seg-btn" :title="t('Exit')" @click="handleLeaveLive">
                <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                  <path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4" />
                  <polyline points="16 17 21 12 16 7" />
                  <line x1="21" y1="12" x2="9" y2="12" />
                </svg>
                <span>{{ t('Exit') }}</span>
              </button>
            </div>
          </div>
        </div>
        <!-- Unified control overlay: shows/hides together with PlayerControl via .controls-visible -->
        <div class="biz-control-overlay">
          <!-- Progress bar at top of control area -->
          <div class="biz-progress-bar">
            <div class="biz-progress-track">
              <div class="biz-progress-fill" />
              <div class="biz-progress-thumb" />
            </div>
          </div>
          <!-- Custom control bar with pill groups -->
          <div class="biz-custom-controls">
            <!-- Left pill: play + audio + volume slider -->
            <div
              class="biz-pill biz-pill-left"
              :class="{ 'volume-expanded': volumeSliderVisible }"
            >
              <div class="biz-tooltip-wrap">
                <button class="biz-pill-btn biz-play-btn" :title="playTooltipText" @click="handleBizPlayPause">
                  <svg v-if="isPlaying" viewBox="0 0 24 24" fill="currentColor"><path d="M6 4h4v16H6V4zm8 0h4v16h-4V4z" /></svg>
                  <svg v-else viewBox="0 0 24 24" fill="currentColor"><path d="M8 5v14l11-7L8 5z" /></svg>
                </button>
                <div class="biz-btn-tooltip">{{ playTooltipText }}</div>
              </div>
              <div class="biz-tooltip-wrap">
                <button class="biz-pill-btn biz-refresh-btn" :title="t('Refresh')" :disabled="isManualRefreshing" @click="handleBizRefresh">
                  <svg viewBox="0 0 24 24" fill="none" aria-hidden="true">
                    <path d="M20 12a8 8 0 0 0-13.66-5.66" />
                    <polyline points="6.2 3.6 6.45 7.7 10.55 7.45" />
                    <path d="M4 12a8 8 0 0 0 13.66 5.66" />
                    <polyline points="17.8 20.4 17.55 16.3 13.45 16.55" />
                  </svg>
                </button>
                <div class="biz-btn-tooltip">{{ t('Refresh') }}</div>
              </div>
              <div
                class="biz-audio-hover-zone"
                @mouseenter="handleVolumeAreaEnter"
                @mouseleave="handleVolumeAreaLeave"
              >
                <div class="biz-tooltip-wrap">
                  <button
                    class="biz-pill-btn biz-audio-btn"
                    :title="muteTooltipText"
                    @mousedown="lockVolumeInteraction"
                    @click="handleBizMuteToggle"
                  >
                    <svg v-if="isMuted" viewBox="0 0 24 24" fill="currentColor"><path d="M16.5 12c0-1.77-1.02-3.29-2.5-4.03v2.21l2.45 2.45c.03-.2.05-.41.05-.63zm2.5 0c0 .94-.2 1.82-.54 2.64l1.51 1.51C20.63 14.91 21 13.5 21 12c0-4.28-2.99-7.86-7-8.77v2.06c2.89.86 5 3.54 5 6.71zM4.27 3L3 4.27 7.73 9H3v6h4l5 5v-6.73l4.25 4.25c-.67.52-1.42.93-2.25 1.18v2.06c1.38-.31 2.63-.95 3.69-1.81L19.73 21 21 19.73l-9-9L4.27 3zM12 4L9.91 6.09 12 8.18V4z" /></svg>
                    <svg v-else viewBox="0 0 24 24" fill="currentColor"><path d="M3 9v6h4l5 5V4L7 9H3zm13.5 3c0-1.77-1.02-3.29-2.5-4.03v8.05c1.48-.73 2.5-2.25 2.5-4.02zM14 3.23v2.06c2.89.86 5 3.54 5 6.71s-2.11 5.85-5 6.71v2.06c4.01-.91 7-4.49 7-8.77s-2.99-7.86-7-8.77z" /></svg>
                  </button>
                  <div class="biz-btn-tooltip">{{ muteTooltipText }}</div>
                </div>
                <div
                  class="biz-volume-slider-area"
                  :class="{ open: volumeSliderVisible }"
                >
                  <div
                    ref="volumeTrackRef"
                    class="biz-volume-track"
                    :style="{ '--biz-volume-percent': volumePercent }"
                    @mousedown="onVolumeTrackMouseDown"
                  >
                    <div class="biz-volume-fill" :style="{ width: 'calc(var(--biz-volume-percent) * 1%)' }" />
                    <div class="biz-volume-thumb" />
                    <div class="biz-volume-value" :class="{ visible: volumeValueVisible }">
                      {{ displayVolumePercent }}
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <!-- Right pill: resolution + PiP + fullscreen -->
            <div class="biz-pill biz-pill-right">
              <div v-if="availableResolutions.length > 0" class="biz-resolution-wrapper">
                <div class="biz-tooltip-wrap" :class="{ 'tooltip-hidden': resolutionPopupVisible }">
                  <button class="biz-pill-btn biz-resolution-btn" :title="resolutionTooltipText" @click="toggleResolutionPopup">
                    <span class="biz-resolution-text">{{ currentResolutionLabel }}</span>
                  </button>
                  <div class="biz-btn-tooltip">{{ resolutionTooltipText }}</div>
                </div>
                <!-- Resolution popup -->
                <Transition name="biz-popup-fade">
                  <div v-if="resolutionPopupVisible" class="biz-resolution-popup">
                    <div
                      v-for="opt in availableResolutions"
                      :key="opt.value"
                      class="biz-resolution-option"
                      :class="{ active: sdkCurrentResolution && opt.value === sdkCurrentResolution.value }"
                      @click="selectResolution(opt)"
                    >
                      {{ opt.label }}
                    </div>
                  </div>
                </Transition>
              </div>
              <div class="biz-tooltip-wrap">
                <button class="biz-pill-btn biz-pip-btn" :title="pipTooltipText" @click="handleBizPiP">
                  <svg viewBox="0 0 24 24" fill="currentColor"><path d="M19 7h-8v6h8V7zm2-4H3c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h18c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm0 16H3V5h18v14z" /></svg>
                </button>
                <div class="biz-btn-tooltip">{{ pipTooltipText }}</div>
              </div>
              <div class="biz-tooltip-wrap">
                <button class="biz-pill-btn biz-cinema-btn" :class="{ active: sidePanelCollapsed }" :title="cinemaTooltipText" @click="handleCinemaMode">
                  <svg class="biz-cinema-icon" viewBox="0 0 24 24" aria-hidden="true">
                    <rect x="3.5" y="5.5" width="17" height="13" rx="2.2" />
                    <path d="M7.5 10.2h9" />
                    <path d="M7.5 13.8h9" />
                    <path d="M17 8.5h1.5v8H17" />
                  </svg>
                </button>
                <div class="biz-btn-tooltip">{{ cinemaTooltipText }}</div>
              </div>
              <div class="biz-tooltip-wrap">
                <button class="biz-pill-btn biz-fullscreen-btn" :title="fullscreenTooltipText" @click="handleBizFullscreen">
                  <svg v-if="!bizIsFullscreen" viewBox="0 0 24 24" fill="currentColor"><path d="M7 14H5v5h5v-2H7v-3zm-2-4h2V7h3V5H5v5zm12 7h-3v2h5v-5h-2v3zM14 5v2h3v3h2V5h-5z" /></svg>
                  <svg v-else viewBox="0 0 24 24" fill="currentColor"><path d="M5 16h3v3h2v-5H5v2zm3-8H5v2h5V5H8v3zm6 11h2v-3h3v-2h-5v5zm2-11V5h-2v5h5V8h-3z" /></svg>
                </button>
                <div class="biz-btn-tooltip">{{ fullscreenTooltipText }}</div>
              </div>
            </div>
          </div>
        </div>
        <Transition name="biz-autoplay-overlay-fade">
          <div
            v-if="autoPlayPromptVisible && !liveEndedOverlayVisible"
            class="biz-autoplay-overlay"
          >
            <div class="biz-autoplay-content">
              <p>{{ t('Content is ready. Click the button to start playback') }}</p>
              <button class="biz-autoplay-action" @click="handleAutoPlayPromptConfirm">
                <svg viewBox="0 0 24 24" aria-hidden="true">
                  <path d="M8 5v14l11-7L8 5z" />
                </svg>
                <span>{{ t('Play') }}</span>
              </button>
            </div>
          </div>
        </Transition>
        <div v-if="liveEndedOverlayVisible" class="live-ended-overlay">
          <div class="live-ended-content">
            <div class="live-ended-icon-wrapper">
              <svg class="live-ended-svg" width="64" height="64" viewBox="0 0 64 64" fill="none" xmlns="http://www.w3.org/2000/svg">
                <circle cx="32" cy="32" r="28" stroke="currentColor" stroke-width="2" opacity="0.15" />
                <circle cx="32" cy="32" r="20" stroke="currentColor" stroke-width="2" opacity="0.25" />
                <path d="M26 24v16l14-8-14-8z" fill="currentColor" opacity="0.4" />
              </svg>
            </div>
            <div class="live-ended-title">
              {{ t('The host is not currently live') }}
            </div>
            <button class="live-ended-back-btn" @click="handleLeaveLive">
              {{ t('Back to live list') }}
            </button>
          </div>
        </div>
      </div>
    </div>

    <!-- Right: Side panel wrapper with toggle handle -->
    <div class="biz-panel-wrapper" :class="{ collapsed: sidePanelCollapsed }">
      <div class="biz-panel-content">
        <BusinessSidePanel :live-ended="liveEndedOverlayVisible" />
      </div>
    </div>

    <TUIDialog
      v-model:visible="exitLiveDialogVisible"
      :title="t('Exit Live')"
    >
      {{ exitDialogContent }}
      <template #footer>
        <div class="action-buttons">
          <TUIButton
            color="gray"
            @click="handleCancelExit"
          >
            {{ t('Cancel') }}
          </TUIButton>
          <TUIButton
            v-if="isUserOnSeat"
            color="red"
            @click="handleEndCoGuest"
          >
            {{ t('End Co-guest') }}
          </TUIButton>
          <TUIButton
            type="primary"
            color="red"
            @click="handleExitLive"
          >
            {{ t('Exit Live') }}
          </TUIButton>
        </div>
      </template>
    </TUIDialog>

    <!-- Seat application dialogs (connection type, device selection, cancel, leave) -->
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
import { ref, onMounted, computed, onUnmounted, watch } from 'vue';
import TUIRoomEngine, { TUIAutoPlayCallbackInfo, TUIRoomEvents } from '@tencentcloud/tuiroom-engine-js';
import {
  TUIMessageBox,
  TUIToast,
  useUIKit,
  TUIDialog,
} from '@tencentcloud/uikit-base-component-vue3';
import {
  LiveView,
  useLiveListState,
  Avatar,
  useRoomEngine,
  LiveListEvent,
  UIKitModal,
} from 'tuikit-atomicx-vue3';
import BusinessSidePanel from './BusinessSidePanel.vue';
import { errorHandler } from '../../TUILiveKit/utils/errorHandler';
import { useSeatApplication } from '../../TUILiveKit/component/SeatApplication/useSeatApplication';
import LiveConnectionTypeDialog from '../../TUILiveKit/component/LiveDialog/LiveConnectionTypeDialog.vue';
import LiveDeviceSelectionDialog from '../../TUILiveKit/component/LiveDialog/LiveDeviceSelectionDialog.vue';
import { usePlayerControlState } from '../composables/usePlayerControlState';
import { initRoomEngineLanguage } from '../../utils/utils';

const { t } = useUIKit();

const {
  isPlaying,
  isMuted: sdkIsMuted,
  isPictureInPicture,
  currentVolume: sdkCurrentVolume,
  currentResolution: sdkCurrentResolution,
  resolutionList: sdkResolutionList,
  resume: sdkResume,
  pause: sdkPause,
  setVolume: sdkSetVolume,
  mute: sdkMute,
  unmute: sdkUnmute,
  requestPictureInPicture: sdkRequestPiP,
  exitPictureInPicture: sdkExitPiP,
  switchResolution: sdkSwitchResolution,
  refresh: sdkRefresh,
} = usePlayerControlState();

// Side panel collapse state
const sidePanelCollapsed = ref(false);

// Mute / volume state — driven by SDK
const isMuted = sdkIsMuted;
const resolutionPopupVisible = ref(false);
const volumePercent = computed(() => sdkCurrentVolume.value);
const volumeSliderVisible = ref(false);
const volumeTrackRef = ref<HTMLElement | null>(null);
const isVolumeAreaHovered = ref(false);
const isVolumeInteracting = ref(false);
let volumeHideTimer: ReturnType<typeof setTimeout> | null = null;

// Resolution — derived from SDK state
const currentResolutionLabel = computed(() => sdkCurrentResolution.value?.label || '');
const availableResolutions = computed(() => sdkResolutionList.value);
const displayVolumePercent = computed(() => `${Math.round(volumePercent.value)}`);
const volumeValueVisible = computed(() => volumeSliderVisible.value && (isVolumeAreaHovered.value || isVolumeInteracting.value));

const showVolumeSlider = () => {
  cancelVolumeHideTimer();
  volumeSliderVisible.value = true;
};

const startVolumeHideTimer = (delay = 220) => {
  cancelVolumeHideTimer();
  volumeHideTimer = setTimeout(() => {
    if (isVolumeAreaHovered.value || isVolumeInteracting.value) return;
    volumeSliderVisible.value = false;
  }, delay);
};

const cancelVolumeHideTimer = () => {
  if (volumeHideTimer) {
    clearTimeout(volumeHideTimer);
    volumeHideTimer = null;
  }
};

const applyVolume = (percent: number) => {
  const clamped = Math.max(0, Math.min(100, percent));
  sdkSetVolume(clamped);
  if (clamped === 0) {
    sdkMute();
  } else if (sdkIsMuted.value) {
    sdkUnmute();
  }
};

const onVolumeTrackMouseDown = (e: MouseEvent) => {
  lockVolumeInteraction();
  cancelVolumeHideTimer();
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
    unlockVolumeInteraction();
    startVolumeHideTimer(400);
  };
  document.addEventListener('mousemove', onMouseMove);
  document.addEventListener('mouseup', onMouseUp);
};

/**
 * Toggle play/pause via SDK API.
 */
const handleBizPlayPause = () => {
  if (isPlaying.value) {
    sdkPause();
  } else {
    sdkResume();
  }
};

/**
 * Toggle mute via SDK API.
 */
const handleBizMuteToggle = () => {
  lockVolumeInteraction();
  showVolumeSlider();
  if (isMuted.value) {
    sdkUnmute();
  } else {
    sdkMute();
  }
  unlockVolumeInteraction();
};

const handleVolumeAreaEnter = () => {
  isVolumeAreaHovered.value = true;
  showVolumeSlider();
};

const handleVolumeAreaLeave = () => {
  isVolumeAreaHovered.value = false;
  startVolumeHideTimer(220);
};

const lockVolumeInteraction = () => {
  isVolumeInteracting.value = true;
  cancelVolumeHideTimer();
  volumeSliderVisible.value = true;
};

const unlockVolumeInteraction = () => {
  window.setTimeout(() => {
    isVolumeInteracting.value = false;
    if (!isVolumeAreaHovered.value) {
      startVolumeHideTimer(280);
    }
  }, 120);
};

/**
 * Toggle picture-in-picture via SDK API.
 */
const handleBizPiP = () => {
  if (isPictureInPicture.value) {
    sdkExitPiP();
  } else {
    sdkRequestPiP();
  }
};

const handleBizRefresh = async () => {
  if (isManualRefreshing.value) return;
  isManualRefreshing.value = true;
  clearManualRefreshTimer();
  manualRefreshTimeoutTimer = setTimeout(() => {
    finishManualRefreshing();
  }, 12000);
  try {
    await sdkRefresh();
    startVideoReadyProbe();
    setTimeout(() => {
      finishManualRefreshing();
    }, 1800);
  } catch (error) {
    console.error('Failed to refresh playback:', error);
    finishManualRefreshing();
  }
};

const handleCinemaMode = () => {
  sidePanelCollapsed.value = !sidePanelCollapsed.value;
};

// Native fullscreen state for biz-video-card (overrides SDK fullscreen)
const bizIsFullscreen = ref(false);
const playTooltipText = computed(() => (isPlaying.value ? t('Pause playback') : t('Resume playback')));
const muteTooltipText = computed(() => (isMuted.value ? t('Turn on sound') : t('Mute sound')));
const pipTooltipText = computed(() => (isPictureInPicture.value ? t('Exit picture in picture') : t('Picture in picture')));
const cinemaTooltipText = computed(() => (sidePanelCollapsed.value ? t('Exit cinema mode') : t('Enter cinema mode')));
const fullscreenTooltipText = computed(() => (bizIsFullscreen.value ? t('Exit full screen') : t('Enter full screen')));
const resolutionTooltipText = computed(() => t('Switch resolution'));

/**
 * Toggle fullscreen using native Fullscreen API on the biz-video-card element.
 * This ensures the custom control overlay stays visible in fullscreen mode.
 */
const handleBizFullscreen = async () => {
  const el = bizVideoCardRef.value;
  if (!el) return;
  if (bizIsFullscreen.value) {
    try {
      await document.exitFullscreen();
    } catch { /* ignore */ }
  } else {
    try {
      await el.requestFullscreen();
    } catch { /* ignore */ }
  }
};

/**
 * Listen for fullscreenchange to keep bizIsFullscreen in sync.
 */
const onFullscreenChange = () => {
  bizIsFullscreen.value = !!document.fullscreenElement;
};

/**
 * Toggle the resolution popup.
 */
const toggleResolutionPopup = () => {
  resolutionPopupVisible.value = !resolutionPopupVisible.value;
};

/**
 * Select a resolution option via SDK API and close popup.
 */
const selectResolution = (opt: { label: string; value: number }) => {
  sdkSwitchResolution(opt);
  resolutionPopupVisible.value = false;
};

/**
 * Close resolution popup when clicking outside the wrapper.
 */
const handleResolutionOutsideClick = (e: MouseEvent) => {
  const wrapper = document.querySelector('.biz-resolution-wrapper');
  if (wrapper && !wrapper.contains(e.target as Node)) {
    resolutionPopupVisible.value = false;
  }
};

const { currentLive, joinLive, leaveLive, subscribeEvent, unsubscribeEvent } = useLiveListState();
const isInLive = computed(() => !!currentLive.value?.liveId);
const roomEngine = useRoomEngine();
let autoPlayListenerBound = false;

function bindAutoPlayFailedListener() {
  if (!roomEngine.instance || autoPlayListenerBound) {
    return;
  }
  roomEngine.instance.on(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
  autoPlayListenerBound = true;
}

const props = defineProps<{
  liveId: string;
}>();

const {
  handleApplyForSeat,
  isUserOnSeat,
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
  handleCancelApplicationConfirm,
  handleCancelApplicationCancel,
  closeLeaveSeatDialog,
  initAutoSelectDevice,
  subscribeEvents,
  unsubscribeEvents,
} = useSeatApplication();

const exitDialogContent = computed(() => (isUserOnSeat.value
  ? t('LiveExitConfirmCoGuestTip')
  : t('Currently connected, do you need to "exit connection" or "end live broadcast"')));

const liveContainerRef = ref<HTMLElement | null>(null);
const bizVideoCardRef = ref<HTMLElement | null>(null);
const liveEndedOverlayVisible = ref(false);
const exitLiveDialogVisible = ref(false);
const autoPlayFailedHandled = ref(false);
const autoPlayPromptVisible = ref(false);
const isManualRefreshing = ref(false);
const readyEmitted = ref(false);
let videoReadyProbeTimer: ReturnType<typeof setInterval> | null = null;
let observedVideoEl: HTMLVideoElement | null = null;
let autoPlayResumeAction: (() => void) | null = null;
let manualRefreshTimeoutTimer: ReturnType<typeof setTimeout> | null = null;

/**
 * Safety timeout (ms) after joinLive succeeds.  If the video never becomes
 * playable (e.g. autoplay blocked on page refresh and the onAutoPlayFailed
 * event fired before the listener was bound), we emit ready and show the
 * play confirmation dialog so the user is not stuck on loading forever.
 */
const READY_SAFETY_TIMEOUT_MS = 6000;
let readySafetyTimer: ReturnType<typeof setTimeout> | null = null;

function clearReadySafetyTimer() {
  if (readySafetyTimer) {
    clearTimeout(readySafetyTimer);
    readySafetyTimer = null;
  }
}

function startReadySafetyTimer() {
  clearReadySafetyTimer();
  readySafetyTimer = setTimeout(() => {
    if (!readyEmitted.value) {
      emitReadyOnce();
      finishManualRefreshing();
      showAutoPlayPrompt(() => sdkResume());
    }
  }, READY_SAFETY_TIMEOUT_MS);
}

function showAutoPlayPrompt(resumeAction: () => void) {
  if (autoPlayFailedHandled.value) return;
  autoPlayFailedHandled.value = true;
  autoPlayResumeAction = resumeAction;
  autoPlayPromptVisible.value = true;
}

function handleAutoPlayPromptConfirm() {
  const resume = autoPlayResumeAction;
  autoPlayResumeAction = null;
  autoPlayPromptVisible.value = false;
  autoPlayFailedHandled.value = false;
  finishManualRefreshing();
  resume?.();
}

function clearManualRefreshTimer() {
  if (manualRefreshTimeoutTimer) {
    clearTimeout(manualRefreshTimeoutTimer);
    manualRefreshTimeoutTimer = null;
  }
}

function finishManualRefreshing() {
  isManualRefreshing.value = false;
  clearManualRefreshTimer();
}

// Hover show/hide for control overlay
const showBizControls = ref(false);
let bizControlsHideTimer: ReturnType<typeof setTimeout> | null = null;

const startBizControlsHideTimer = () => {
  cancelBizControlsHideTimer();
  bizControlsHideTimer = setTimeout(() => {
    showBizControls.value = false;
  }, 2000);
};

const cancelBizControlsHideTimer = () => {
  if (bizControlsHideTimer) {
    clearTimeout(bizControlsHideTimer);
    bizControlsHideTimer = null;
  }
};

const displayHostName = computed(() => currentLive.value?.liveOwner.userName || currentLive.value?.liveOwner.userId || 'Speaker');
const displayRoomTitle = computed(() => {
  const title = (currentLive.value?.liveName || '').trim();
  if (title) return title;
  return displayHostName.value;
});

const emit = defineEmits(['leaveLive', 'ready']);

function emitReadyOnce() {
  if (readyEmitted.value) return;
  readyEmitted.value = true;
  clearReadySafetyTimer();
  emit('ready');
}

function isVideoReady(videoEl: HTMLVideoElement): boolean {
  return videoEl.readyState >= 3 && videoEl.videoWidth > 0 && videoEl.videoHeight > 0;
}

function tryEmitReadyFromVideo() {
  if (observedVideoEl && isVideoReady(observedVideoEl)) {
    finishManualRefreshing();
    emitReadyOnce();
  }
}

function bindVideoReadyListeners(videoEl: HTMLVideoElement) {
  if (observedVideoEl === videoEl) return;
  unbindVideoReadyListeners();
  observedVideoEl = videoEl;
  videoEl.addEventListener('loadeddata', tryEmitReadyFromVideo);
  videoEl.addEventListener('canplay', tryEmitReadyFromVideo);
  videoEl.addEventListener('playing', tryEmitReadyFromVideo);
  tryEmitReadyFromVideo();
}

function unbindVideoReadyListeners() {
  if (!observedVideoEl) return;
  observedVideoEl.removeEventListener('loadeddata', tryEmitReadyFromVideo);
  observedVideoEl.removeEventListener('canplay', tryEmitReadyFromVideo);
  observedVideoEl.removeEventListener('playing', tryEmitReadyFromVideo);
  observedVideoEl = null;
}

function startVideoReadyProbe() {
  if (videoReadyProbeTimer) {
    clearInterval(videoReadyProbeTimer);
  }
  videoReadyProbeTimer = setInterval(() => {
    const root = bizVideoCardRef.value;
    if (!root) return;
    const videoEl = root.querySelector('video');
    if (videoEl instanceof HTMLVideoElement) {
      bindVideoReadyListeners(videoEl);
      if (isVideoReady(videoEl) && videoReadyProbeTimer) {
        clearInterval(videoReadyProbeTimer);
        videoReadyProbeTimer = null;
      }
    }
  }, 120);
}

const handleLiveEnded = () => {
  liveEndedOverlayVisible.value = true;
  emitReadyOnce();
};

const handleKickedOutOfLive = () => {
  TUIMessageBox.alert({
    title: t('Unable to watch live'),
    content: t('You have been removed from the live room and cannot watch the live stream'),
    confirmText: t('Back to home'),
    showClose: false,
    modal: false,
    callback: () => {
      emit('leaveLive');
    },
  });
};

// Auto-select devices when device selection dialog opens
watch(deviceSelectionDialogVisible, (val) => {
  if (val) {
    initAutoSelectDevice();
  }
});

onMounted(async () => {
  startVideoReadyProbe();
  bindAutoPlayFailedListener();
  if (!autoPlayListenerBound) {
    TUIRoomEngine.once('ready', bindAutoPlayFailedListener);
  }
  subscribeEvent(LiveListEvent.onLiveEnded, handleLiveEnded);
  subscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  subscribeEvents();
  await initRoomEngineLanguage();
  const joined = await handleJoinLive();
  if (!joined) {
    emitReadyOnce();
  } else {
    // Start safety timer: if the video never becomes playable within the
    // timeout (e.g. autoplay blocked on page refresh), auto-emit ready
    // and show a play confirmation dialog.
    startReadySafetyTimer();
  }
  // Close resolution popup on outside click
  document.addEventListener('click', handleResolutionOutsideClick);

  // Listen for native fullscreen changes
  document.addEventListener('fullscreenchange', onFullscreenChange);
});

onUnmounted(async () => {
  unsubscribeEvent(LiveListEvent.onLiveEnded, handleLiveEnded);
  unsubscribeEvent(LiveListEvent.onKickedOutOfLive, handleKickedOutOfLive);
  unsubscribeEvents();
  if (currentLive.value?.liveId) {
    await leaveLive();
  }
  if (autoPlayListenerBound) {
    roomEngine.instance?.off(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
    autoPlayListenerBound = false;
  }
  cancelBizControlsHideTimer();
  cancelVolumeHideTimer();
  clearReadySafetyTimer();
  clearManualRefreshTimer();
  if (videoReadyProbeTimer) {
    clearInterval(videoReadyProbeTimer);
    videoReadyProbeTimer = null;
  }
  unbindVideoReadyListeners();
  document.removeEventListener('click', handleResolutionOutsideClick);
  document.removeEventListener('fullscreenchange', onFullscreenChange);
});

function handleCopyLink() {
  const url = window.location.href;
  navigator.clipboard.writeText(url)
    .then(() => {
      TUIToast.success({ message: t('Link copied') });
    })
    .catch(() => {
      TUIToast.error({ message: t('Copy failed') });
    });
}

function handleLeaveLive() {
  if (isUserOnSeat.value) {
    exitLiveDialogVisible.value = true;
  } else {
    emit('leaveLive');
  }
}

function handleExitLive() {
  exitLiveDialogVisible.value = false;
  emit('leaveLive');
}

async function handleEndCoGuest() {
  exitLiveDialogVisible.value = false;
  try {
    await confirmLeaveSeat();
  } catch (error) {
    console.error('Failed to leave seat:', error);
    TUIToast.error({
      message: t('Failed to leave seat'),
    });
  }
}

function handleCancelExit() {
  exitLiveDialogVisible.value = false;
}

function showErrorAndLeave(content: string) {
  TUIMessageBox.alert({
    title: t('Unable to watch live'),
    content,
    confirmText: t('Back to home'),
    showClose: false,
    modal: false,
    callback: () => {
      emit('leaveLive');
    },
  });
}

async function handleJoinLive(): Promise<boolean> {
  if (props.liveId && props.liveId.trim()) {
    try {
      await joinLive({ liveId: props.liveId });
      return true;
    } catch (error: any) {
      console.error('Failed to join live room, error:', error);
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
  } else {
    console.error('liveId is empty');
    showErrorAndLeave(t('LiveId is empty'));
    return false;
  }
}

function handleAutoPlayFailed(event: TUIAutoPlayCallbackInfo) {
  // Autoplay failed means media is already ready, only blocked by browser policy.
  // Emit ready to avoid page-level loading getting stuck.
  emitReadyOnce();
  finishManualRefreshing();
  showAutoPlayPrompt(() => event.resume());
}

watch(isPlaying, (playing) => {
  if (playing) {
    finishManualRefreshing();
    emitReadyOnce();
    autoPlayPromptVisible.value = false;
    autoPlayResumeAction = null;
    autoPlayFailedHandled.value = false;
  }
}, { immediate: true });
</script>

<style lang="scss" scoped>
.live-player-business-pc {
  display: flex;
  width: 100%;
  height: 100%;
  min-height: 0;
  min-width: 0;
  background: var(--preset-bg-base);
  overflow: hidden;
  color: var(--preset-text-primary);
}

.biz-left-column {
  flex: 1;
  min-height: 0;
  min-width: 0;
}

.biz-video-card {
  width: 100%;
  height: 100%;
  min-height: 0;
  position: relative;
  border-radius: 0;
  overflow: hidden;
  background: var(--preset-video-bg);
  border-right: 1px solid var(--preset-stage-divider, rgba(255, 255, 255, 0.08));

  :deep(> *:first-child) {
    position: absolute !important;
    inset: 0 !important;
    width: 100% !important;
    height: 100% !important;
    z-index: 2;
  }

  :deep(video) {
    width: 100% !important;
    height: 100% !important;
    object-fit: cover !important;
    transform: translateZ(0) scale(1);
    transform-origin: center center;
    backface-visibility: hidden;
    will-change: transform;
    transition: transform 560ms cubic-bezier(0.16, 1, 0.3, 1);
  }

  :deep(.live-core-ui) {
    z-index: 10;
  }

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

  .biz-video-top-gradient {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 140px;
    z-index: 42;
    pointer-events: none;
    opacity: 0;
    transition: opacity 0.28s ease;
    background: linear-gradient(
      180deg,
      var(--preset-video-top-gradient-start, rgba(2, 6, 23, 0.85)) 0%,
      var(--preset-video-top-gradient-mid, rgba(2, 6, 23, 0.15)) 58%,
      var(--preset-video-top-gradient-end, rgba(2, 6, 23, 0)) 100%
    );
  }

  .biz-video-top-overlay {
    position: absolute;
    top: 20px;
    left: 20px;
    right: 20px;
    z-index: 44;
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 16px;
    pointer-events: none;
    opacity: 0;
    transform: translateY(-6px);
    transition: opacity 0.28s ease, transform 0.28s ease;
  }

  .stream-top-unified {
    width: 100%;
    margin-left: 0;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    padding: 7px;
    border-radius: 18px;
    pointer-events: auto;
    background: var(--preset-stream-glass-bg, rgba(9, 17, 35, 0.62));
    border: 1px solid var(--preset-glass-border);
    backdrop-filter: blur(10px);
    box-shadow: var(--preset-glass-shadow);
  }

  .stream-meta-glass,
  .stream-actions-glass {
    pointer-events: auto;
    background: transparent;
    border: 0;
    backdrop-filter: none;
    box-shadow: none;
  }

  .stream-meta-glass {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 2px 6px 2px 4px;
    border-radius: 14px;
    min-width: 0;
    flex: 1;
    max-width: min(620px, 72%);
  }

  .stream-host-avatar {
    position: relative;
    flex-shrink: 0;
  }

  .stream-host-avatar-img {
    border-radius: 999px;
    overflow: hidden;
  }

  .stream-meta-text {
    min-width: 0;
    display: flex;
    flex-direction: column;
    gap: 4px;
  }

  .stream-title-row {
    display: flex;
    align-items: center;
    gap: 10px;
    min-width: 0;
  }

  .stream-live-badge {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    padding: 2px 9px;
    border-radius: 999px;
    background: var(--preset-live-badge-bg, rgba(22, 163, 74, 0.28));
    border: 1px solid var(--preset-live-badge-border, rgba(74, 222, 128, 0.52));
    color: var(--preset-live-badge-text, #dcfce7);
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 0.1em;
    flex-shrink: 0;
  }

  .stream-live-badge-dot {
    width: 6px;
    height: 6px;
    border-radius: 999px;
    background: var(--preset-live-badge-dot, #86efac);
    box-shadow: 0 0 8px var(--preset-live-badge-dot-shadow, rgba(134, 239, 172, 0.65));
  }

  .stream-title {
    color: var(--preset-stream-title, #ffffff);
    font-size: 15px;
    font-weight: 700;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 100%;
    letter-spacing: 0.01em;
  }

  .stream-subtitle {
    display: flex;
    align-items: center;
    gap: 6px;
    color: var(--preset-stream-subtitle, rgba(226, 232, 240, 0.86));
    font-size: 12px;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 100%;
  }

  .stream-subtitle-host {
    color: var(--preset-stream-subtitle-host, #cbd5e1);
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .stream-subtitle-sep {
    color: var(--preset-stream-subtitle-sep, rgba(148, 163, 184, 0.7));
    flex-shrink: 0;
  }

  .stream-subtitle-room {
    color: var(--preset-stream-subtitle-room, rgba(148, 163, 184, 0.88));
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .stream-actions-glass {
    display: flex;
    align-items: center;
    gap: 0;
    border-radius: 14px;
    padding: 0;
    border-left: 1px solid var(--preset-stage-divider, rgba(255, 255, 255, 0.08));
    flex-shrink: 0;
  }

  .top-action-segmented {
    height: 38px;
    border-radius: 14px;
    border: 1px solid rgba(255, 255, 255, 0.1);
    background: rgba(20, 24, 33, 0.88);
    box-shadow: 0 10px 24px rgba(2, 6, 23, 0.3);
    overflow: hidden;
    margin-left: 8px;
  }

  .top-seg-btn {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    justify-content: center;
    height: 100%;
    min-width: 124px;
    border: 0;
    border-right: 1px solid rgba(255, 255, 255, 0.08);
    padding: 0 16px;
    font-size: 14px;
    font-weight: 700;
    color: #ffffff;
    background: transparent;
    cursor: pointer;
    transition: background 180ms ease, color 180ms ease;

    &:last-child {
      border-right: 0;
    }

    &:hover {
      color: #ffffff;
      background: rgba(255, 255, 255, 0.05);
    }

    svg {
      opacity: 0.95;
    }

    span {
      line-height: 1;
    }
  }

  .top-seg-btn-primary {
    color: #ffffff;
    background: rgba(255, 255, 255, 0.12);

    &:hover {
      color: #ffffff;
      background: rgba(255, 255, 255, 0.18);
    }
  }

  .biz-control-overlay {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    z-index: 30;
    pointer-events: none;
    opacity: 0;
    transform: translateY(8px);
    transition: opacity 0.3s ease, transform 0.3s ease;
  }

  &.controls-visible .biz-control-overlay {
    opacity: 1;
    transform: translateY(0);
    z-index: 40;

    > * {
      pointer-events: auto;
    }
  }

  &.controls-visible {
    .biz-video-top-gradient {
      opacity: 1;
    }

    .biz-video-top-overlay {
      opacity: 1;
      transform: translateY(0);
    }
  }

  .biz-progress-bar {
    position: absolute;
    bottom: 76px;
    left: 16px;
    right: 16px;
    z-index: 14;
    padding: 6px 0;
    cursor: pointer;

    .biz-progress-track {
      position: relative;
      width: 100%;
      height: 5px;
      background: var(--preset-progress-track-bg, rgba(255, 255, 255, 0.2));
      border-radius: 9999px;

      .biz-progress-fill {
        height: 100%;
        width: 100%;
        background: linear-gradient(
          90deg,
          var(--preset-progress-fill-start, #e51c1c) 0%,
          var(--preset-progress-fill-end, #f63b3b) 100%
        );
        border-radius: 9999px;
      }

      .biz-progress-thumb {
        display: none;
      }
    }

    &:hover .biz-progress-track {
      height: 7px;

      .biz-progress-thumb {
        display: block;
        position: absolute;
        right: -4px;
        top: 50%;
        transform: translateY(-50%);
        width: 12px;
        height: 12px;
        border-radius: 50%;
        background: var(--preset-progress-thumb-bg, #e51c1c);
        box-shadow: 0 1px 4px var(--preset-progress-thumb-shadow, rgba(229, 28, 28, 0.4));
      }
    }
  }

  .biz-custom-controls {
    display: flex;
    align-items: center;
    justify-content: space-between;
    height: 62px;
    padding: 0 16px 20px;
    box-sizing: border-box;
  }

  .biz-pill {
    display: flex;
    align-items: center;
    gap: 2px;
    padding: 5px 8px;
    border-radius: 30px;
    background: var(--preset-control-pill-bg, rgba(0, 0, 0, 0.45));
    box-shadow: var(--preset-control-pill-shadow, 0 4px 16px rgba(0, 0, 0, 0.15));
    transition: all 0.3s ease;
  }

  .biz-pill-left.volume-expanded {
    gap: 4px;
  }

  .biz-pill-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 46px;
    height: 46px;
    border: none;
    border-radius: 50%;
    background: transparent;
    color: var(--preset-control-btn-color, #ffffff);
    cursor: pointer;
    transition: background 150ms ease;
    padding: 0;
    flex-shrink: 0;

    svg {
      width: 24px;
      height: 24px;
      flex-shrink: 0;
      color: var(--preset-control-btn-color, #ffffff);
      fill: var(--preset-control-btn-color, #ffffff);
      stroke: var(--preset-control-btn-color, #ffffff);
      stroke-width: 0.3;
    }

    &:hover {
      background: var(--preset-control-btn-hover-bg, rgba(255, 255, 255, 0.1));
    }

    &:active {
      transform: scale(0.92);
    }
  }

  .biz-refresh-btn {
    &:disabled {
      opacity: 0.45;
      cursor: not-allowed;
    }

    svg {
      fill: none !important;
      stroke: currentColor;
      stroke-width: 2;
      stroke-linecap: round;
      stroke-linejoin: round;
    }
  }

  .biz-tooltip-wrap {
    position: relative;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    flex-shrink: 0;

    .biz-btn-tooltip {
      position: absolute;
      bottom: calc(100% + 10px);
      left: 50%;
      transform: translateX(-50%) translateY(4px);
      opacity: 0;
      pointer-events: none;
      white-space: nowrap;
      font-size: 12px;
      line-height: 1;
      color: var(--preset-control-tooltip-text, #eef2ff);
      background: var(--preset-control-tooltip-bg, rgba(30, 41, 59, 0.92));
      border: 1px solid var(--preset-control-tooltip-border, rgba(148, 163, 184, 0.32));
      border-radius: 14px;
      padding: 8px 10px;
      box-shadow: var(--preset-control-tooltip-shadow, 0 8px 24px rgba(0, 0, 0, 0.35));
      transition: opacity 180ms ease, transform 180ms ease;
      z-index: 80;
    }

    &:hover .biz-btn-tooltip {
      opacity: 1;
      transform: translateX(-50%) translateY(0);
    }

    &.tooltip-hidden .biz-btn-tooltip {
      opacity: 0 !important;
      transform: translateX(-50%) translateY(4px) !important;
    }
  }

  .biz-resolution-btn {
    width: auto;
    min-width: 46px;
    padding: 0 14px;
    border-radius: 23px;

    .biz-resolution-text {
      font-size: var(--preset-font-size-title);
      font-weight: 650;
      color: var(--preset-control-btn-color, #ffffff);
      white-space: nowrap;
      letter-spacing: 0.5px;
    }
  }

  .biz-cinema-btn {
    &.active {
      background: var(--preset-control-btn-hover-bg, rgba(255, 255, 255, 0.14));
    }

    .biz-cinema-icon {
      width: 27px;
      height: 27px;
      fill: none !important;
      stroke: var(--preset-control-btn-color, #ffffff);
      stroke-width: 1.9;
      stroke-linecap: round;
      stroke-linejoin: round;
    }
  }

  .biz-fullscreen-btn {
    svg {
      width: 27px;
      height: 27px;
    }
  }

  .biz-resolution-wrapper {
    position: relative;
  }

  .biz-resolution-popup {
    position: absolute;
    bottom: calc(100% + 12px);
    left: 50%;
    transform: translateX(-50%);
    display: flex;
    flex-direction: column;
    gap: 2px;
    padding: 6px;
    border-radius: 16px;
    background: var(--preset-resolution-popup-bg, rgba(0, 0, 0, 0.55));
    box-shadow: var(--preset-resolution-popup-shadow, 0 4px 16px rgba(0, 0, 0, 0.15));
    z-index: 100;
    min-width: 72px;

    .biz-resolution-option {
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 6px 16px;
      border-radius: 12px;
      color: var(--preset-resolution-option-text, rgba(255, 255, 255, 0.7));
      font-size: var(--preset-font-size-body);
      font-weight: var(--preset-font-weight-bold);
      cursor: pointer;
      white-space: nowrap;
      transition: all 150ms ease;
      background: transparent;

      &:hover {
        background: var(--preset-resolution-option-hover-bg, rgba(255, 255, 255, 0.1));
        color: var(--preset-resolution-option-hover-text, #ffffff);
      }

      &.active {
        background: var(--preset-resolution-option-active-bg, rgba(255, 255, 255, 0.18));
        color: var(--preset-resolution-option-active-text, #ffffff);
      }
    }
  }

  .biz-popup-fade-enter-active,
  .biz-popup-fade-leave-active {
    transition: opacity 0.2s ease, transform 0.2s ease;
  }

  .biz-popup-fade-enter-from,
  .biz-popup-fade-leave-to {
    opacity: 0;
    transform: translateX(-50%) translateY(6px);
  }

  .biz-audio-hover-zone {
    display: inline-flex;
    align-items: center;
    gap: 0;
  }

  .biz-volume-slider-area {
    display: flex;
    align-items: center;
    width: 0;
    padding: 0 6px 0 2px;
    flex-shrink: 0;
    opacity: 0;
    transform: translateX(-6px);
    pointer-events: none;
    overflow: visible;
    transition: width 240ms cubic-bezier(0.22, 1, 0.36, 1), opacity 200ms ease, transform 240ms ease;

    &.open {
      width: 96px;
      opacity: 1;
      transform: translateX(0);
      pointer-events: auto;
    }

    .biz-volume-track {
      position: relative;
      width: 100%;
      height: 6px;
      background: var(--preset-volume-track-bg, rgba(255, 255, 255, 0.2));
      border-radius: 9999px;
      cursor: pointer;
      --biz-thumb-size: 14px;
      --biz-thumb-half: 7px;

      .biz-volume-fill {
        position: relative;
        z-index: 1;
        height: 100%;
        background: var(--preset-volume-fill-bg, #ffffff);
        border-radius: 9999px;
        transition: width 50ms linear;
      }

      .biz-volume-thumb {
        position: absolute;
        z-index: 3;
        top: 50%;
        transform: translate(-50%, -50%);
        width: var(--biz-thumb-size);
        height: var(--biz-thumb-size);
        border-radius: 50%;
        background: var(--preset-volume-thumb-bg, #ffffff);
        box-shadow: 0 1px 4px var(--preset-volume-thumb-shadow, rgba(0, 0, 0, 0.3));
        cursor: grab;
        left: clamp(
          var(--biz-thumb-half),
          calc(var(--biz-volume-percent, 0) * 1%),
          calc(100% - var(--biz-thumb-half))
        );
        transition: left 50ms linear;

        &:active {
          cursor: grabbing;
          transform: translate(-50%, -50%) scale(1.15);
        }
      }

      .biz-volume-value {
        position: absolute;
        z-index: 4;
        left: clamp(
          var(--biz-thumb-half),
          calc(var(--biz-volume-percent, 0) * 1%),
          calc(100% - var(--biz-thumb-half))
        );
        bottom: calc(100% + 12px);
        transform: translateX(-50%) translateY(4px);
        min-width: 28px;
        height: 20px;
        padding: 0 5px;
        border-radius: 10px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        font-size: 10px;
        font-weight: 600;
        line-height: 1;
        font-variant-numeric: tabular-nums;
        letter-spacing: 0;
        color: var(--preset-control-tooltip-text, #eef2ff);
        background: var(--preset-control-tooltip-bg, rgba(30, 41, 59, 0.92));
        border: 1px solid var(--preset-control-tooltip-border, rgba(148, 163, 184, 0.32));
        box-shadow: var(--preset-control-tooltip-shadow, 0 8px 24px rgba(0, 0, 0, 0.35));
        opacity: 0;
        pointer-events: none;
        transition: opacity 150ms ease, transform 150ms ease;

        &.visible {
          opacity: 1;
          transform: translateX(-50%) translateY(0);
        }
      }
    }
  }

  .live-ended-overlay {
    position: absolute;
    inset: 0;
    background: var(--preset-live-ended-bg, rgba(15, 18, 25, 0.88));
    backdrop-filter: blur(24px);
    z-index: 60;
    display: flex;
    align-items: center;
    justify-content: center;

    .live-ended-content {
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 12px;
      max-width: 320px;
      text-align: center;
    }

    .live-ended-icon-wrapper {
      width: 80px;
      height: 80px;
      display: flex;
      align-items: center;
      justify-content: center;
      margin-bottom: 4px;
    }

    .live-ended-svg {
      color: var(--preset-live-ended-icon, rgba(255, 255, 255, 0.45));
    }

    .live-ended-title {
      font-size: 18px;
      font-weight: 600;
      color: var(--preset-live-ended-title, #ffffff);
      line-height: 1.3;
    }

    .live-ended-subtitle {
      font-size: 14px;
      color: var(--preset-live-ended-subtitle, rgba(255, 255, 255, 0.55));
      line-height: 1.5;
    }

    .live-ended-back-btn {
      margin-top: 8px;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      height: 40px;
      padding: 0 28px;
      background: var(--preset-btn-primary-bg, #3b82f6);
      color: #ffffff;
      border: none;
      border-radius: 9999px;
      font-size: 14px;
      font-weight: 500;
      cursor: pointer;
      transition: all 200ms ease;

      &:hover {
        background: var(--preset-btn-primary-hover, #2563eb);
        transform: scale(1.02);
      }

      &:active {
        transform: scale(0.97);
      }
    }
  }

  .biz-manual-refresh-overlay {
    position: absolute;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 45;
    pointer-events: none;
    background:
      radial-gradient(circle at 18% 18%, rgba(255, 255, 255, 0.06), transparent 44%),
      linear-gradient(180deg, rgba(3, 8, 20, 0.2), rgba(2, 7, 18, 0.3));
    backdrop-filter: blur(3px) saturate(112%);
    -webkit-backdrop-filter: blur(3px) saturate(112%);
  }

  .biz-manual-refresh-spinner {
    position: relative;
    width: 124px;
    height: 124px;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .biz-manual-refresh-orbit {
    position: absolute;
    inset: 0;
    width: 100%;
    height: 100%;

    .orbit-track {
      fill: none;
      stroke: rgba(255, 255, 255, 0.16);
      stroke-width: 6;
    }

    .orbit-segment {
      fill: none;
      stroke: #ffffff;
      stroke-width: 6;
      stroke-linecap: round;
      stroke-dasharray: 68 246;
      stroke-dashoffset: -8;
    }

    .orbit-segment-alt {
      stroke-dashoffset: -165;
    }

    .orbit-segments {
      transform-origin: 64px 64px;
      animation: bizRefreshOrbitSpin 1.8s infinite;
      will-change: transform;
    }
  }

  .biz-manual-refresh-logo {
    width: 34px;
    height: 34px;
    object-fit: contain;
  }

  .biz-autoplay-overlay {
    position: absolute;
    inset: 0;
    z-index: 52;
    display: flex;
    align-items: center;
    justify-content: center;
    pointer-events: auto;
    background:
      radial-gradient(circle at 16% 18%, rgba(255, 226, 160, 0.26), transparent 38%),
      radial-gradient(circle at 14% 24%, rgba(255, 255, 255, 0.14), transparent 42%),
      linear-gradient(180deg, rgba(2, 5, 14, 0.48), rgba(1, 3, 10, 0.74));
    backdrop-filter: blur(3px);
  }

  .biz-autoplay-content {
    width: min(640px, calc(100% - 72px));
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 16px;
    text-align: center;

    p {
      margin: 0;
      color: rgba(248, 250, 252, 0.95);
      font-size: 19px;
      font-weight: 650;
      line-height: 1.42;
      letter-spacing: 0.01em;
      text-shadow: 0 2px 10px rgba(2, 6, 23, 0.45);
    }
  }

  .biz-autoplay-action {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
    height: 38px;
    padding: 0 15px;
    border-radius: 9px;
    border: 1px solid rgba(203, 213, 225, 0.34);
    background: rgba(148, 163, 184, 0.18);
    color: rgba(248, 250, 252, 0.94);
    font-size: 14px;
    font-weight: 600;
    cursor: pointer;
    transition: background 180ms ease, border-color 180ms ease, color 180ms ease, transform 180ms ease;

    svg {
      width: 16px;
      height: 16px;
      fill: currentColor;
    }

    &:hover {
      background: rgba(148, 163, 184, 0.26);
      border-color: rgba(226, 232, 240, 0.5);
      transform: translateY(-1px);
    }

    &:active {
      transform: scale(0.97);
    }
  }
}

.biz-autoplay-overlay-fade-enter-active,
.biz-autoplay-overlay-fade-leave-active {
  transition: opacity 220ms ease;
}

.biz-autoplay-overlay-fade-enter-from,
.biz-autoplay-overlay-fade-leave-to {
  opacity: 0;
}

.biz-manual-refresh-fade-enter-active,
.biz-manual-refresh-fade-leave-active {
  transition: opacity 180ms ease;
}

.biz-manual-refresh-fade-enter-from,
.biz-manual-refresh-fade-leave-to {
  opacity: 0;
}

@keyframes livePulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.4; }
}

@keyframes bizRefreshOrbitSpin {
  0% {
    transform: rotate(0deg);
    animation-timing-function: cubic-bezier(0.42, 0, 0.58, 1);
  }

  50% {
    transform: rotate(180deg);
    animation-timing-function: cubic-bezier(0.42, 0, 0.58, 1);
  }

  100% {
    transform: rotate(360deg);
  }
}

.action-buttons {
  display: flex;
  gap: 10px;
}

.biz-panel-wrapper {
  position: relative;
  display: flex;
  align-items: stretch;
  width: clamp(320px, 25vw, 440px);
  height: 100%;
  min-height: 0;
  min-width: 0;
  overflow: visible;
  transition: width 380ms cubic-bezier(0.22, 1, 0.36, 1);

  .biz-panel-content {
    width: 100%;
    height: 100%;
    min-height: 0;
    overflow: hidden;
    box-sizing: border-box;
    transform: translateX(0);
    transition: transform 360ms cubic-bezier(0.22, 1, 0.36, 1), opacity 280ms ease;
    opacity: 1;
  }

  &.collapsed {
    width: 0;

    .biz-panel-content {
      opacity: 0;
      transform: translateX(18px);
      pointer-events: none;
    }
  }
}

  @media (max-width: 1080px) {
  .live-player-business-pc {
    flex-direction: column;
  }

  .biz-video-card {
    border-right: 0;
    border-bottom: 1px solid var(--preset-stage-divider, rgba(255, 255, 255, 0.08));

    .biz-video-top-overlay {
      left: 12px;
      right: 12px;
      top: 12px;
    }

    .stream-top-unified {
      width: 100%;
      padding: 6px;
      gap: 8px;
      border-radius: 14px;
    }

    .stream-meta-glass {
      max-width: none;
    }

    .stream-actions-glass {
      gap: 6px;
      padding-left: 8px;
    }

    .top-seg-btn span {
      display: none;
    }
  }

  .biz-panel-wrapper {
    width: 100%;
  }

}
</style>
