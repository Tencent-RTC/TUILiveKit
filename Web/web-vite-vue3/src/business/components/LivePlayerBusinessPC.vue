<template>
  <div
    id="liveContainer"
    ref="liveContainerRef"
    class="live-player-business-pc"
    :class="{ 'panel-collapsed': sidePanelCollapsed }"
  >
    <!-- Left: Immersive video stage -->
    <div class="biz-left-column">
      <div class="biz-stage-topbar">
        <div class="biz-video-top-gradient" />
        <div class="biz-video-top-overlay">
          <div class="stream-top-unified">
            <div class="stream-meta-glass">
              <template v-if="liveEndedOverlayVisible">
                <div class="stream-host-avatar">
                  <div class="stream-ended-avatar">
                    <IconUser size="24" />
                  </div>
                </div>
                <div class="stream-meta-text">
                  <div class="stream-title-row">
                    <div class="stream-title">
                      {{ t('The host is not currently live') }}
                    </div>
                  </div>
                </div>
              </template>
              <template v-else>
                <div class="stream-host-avatar">
                  <Avatar
                    :src="currentLive?.liveOwner.avatarUrl"
                    :size="32"
                    class="stream-host-avatar-img"
                  />
                </div>
                <div class="stream-meta-text">
                  <div class="stream-title-row">
                    <div class="stream-title">
                      {{ displayRoomTitle }}
                    </div>
                  </div>
                  <span class="stream-live-badge">
                    <span class="stream-live-badge-dot" />
                    LIVE
                  </span>
                </div>
              </template>
            </div>
            <div class="stream-actions-glass">
              <button class="biz-segment-btn biz-segment-btn-copy" :title="t('Copy Link')" @click="handleCopyLink">
                <span class="biz-segment-label">{{ t('Copy Link') }}</span>
              </button>
              <button
                v-if="isUserOnSeat"
                class="biz-segment-btn biz-segment-btn-disconnect"
                :title="t('End Co-guest')"
                @click="handleDisconnectCoGuest"
              >
                <span class="biz-segment-label">{{ t('End Co-guest') }}</span>
              </button>
              <button class="biz-segment-btn biz-segment-btn-leave" :title="t('Exit')" @click="handleLeaveLive">
                <span class="biz-segment-label">{{ t('Exit') }}</span>
              </button>
            </div>
          </div>
        </div>
      </div>
      <div
        ref="bizVideoCardRef"
        class="biz-video-card"
        :class="{
          'controls-visible-bottom': showBizControls && !liveEndedOverlayVisible && !isUserOnSeat,
        }"
        @mouseenter="handleBizVideoMouseEnter"
        @mousemove="handleBizVideoMouseMove"
        @mouseleave="handleBizVideoMouseLeave"
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
        <!-- Unified control overlay: shows/hides together with PlayerControl via .controls-visible -->
        <div class="biz-control-overlay">
          <!-- Progress bar at top of control area -->
          <div
            class="biz-progress-bar"
            @mouseenter="handleControlOverlayEnter"
            @mousemove="handleControlOverlayMove"
            @mouseleave="handleControlOverlayLeave"
          >
            <div class="biz-progress-track">
              <div class="biz-progress-fill" />
              <div class="biz-progress-thumb" />
            </div>
          </div>
          <!-- Custom control bar with pill groups -->
          <div
            class="biz-custom-controls"
            @mouseenter="handleControlOverlayEnter"
            @mousemove="handleControlOverlayMove"
            @mouseleave="handleControlOverlayLeave"
          >
            <!-- Left pill: play + audio + volume slider -->
            <div
              class="biz-pill biz-pill-left"
              :class="{ 'volume-expanded': volumeSliderVisible }"
            >
              <div class="biz-tooltip-wrap">
                <button class="biz-pill-btn biz-play-btn" :class="{ disabled: playToggleDisabled }" @click="handleBizPlayPause">
                  <svg v-if="isPlaying" viewBox="0 0 24 24" fill="currentColor"><path d="M6 4h4v16H6V4zm8 0h4v16h-4V4z" /></svg>
                  <svg v-else viewBox="0 0 24 24" fill="currentColor"><path d="M8 5v14l11-7L8 5z" /></svg>
                </button>
                <div class="biz-btn-tooltip">{{ playTooltipText }}</div>
              </div>
              <div class="biz-tooltip-wrap">
                <button class="biz-pill-btn biz-refresh-btn" :class="{ disabled: refreshDisabled }" @click="handleBizRefresh">
                  <svg viewBox="0 0 16 16" aria-hidden="true">
                    <path d="M8.00016 2.71134C10.8965 2.71134 13.2534 5.0736 13.2534 7.99868H14.2148C14.2148 4.54763 11.4324 1.75 8.00016 1.75C5.76382 1.75 3.80336 2.93771 2.7087 4.72001L2.7087 2.64252H1.75V5.74894C1.75 6.02508 1.97386 6.24894 2.25 6.24894L5.33676 6.24894V5.28498L3.49045 5.28498C4.40906 3.74211 6.08692 2.71134 8.00016 2.71134Z" />
                    <path d="M1.78554 7.99863H2.74687C2.74687 10.9237 5.10379 13.286 8.00015 13.286C9.91339 13.286 11.5912 12.2553 12.5099 10.7124H10.6632V9.74843H13.75C14.0261 9.74843 14.25 9.97228 14.25 10.2484V13.3548H13.2913V11.2779C12.1966 13.0599 10.2363 14.2474 8.00015 14.2474C4.56792 14.2474 1.78554 11.4497 1.78554 7.99863Z" />
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
                  <button class="biz-pill-btn biz-resolution-btn" @click="toggleResolutionPopup">
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
                <button class="biz-pill-btn biz-pip-btn" :class="{ disabled: pipDisabled }" @click="handleBizPiP">
                  <svg viewBox="0 0 24 24" fill="currentColor"><path d="M19 7h-8v6h8V7zm2-4H3c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h18c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm0 16H3V5h18v14z" /></svg>
                </button>
                <div class="biz-btn-tooltip">{{ pipTooltipText }}</div>
              </div>
              <div class="biz-tooltip-wrap">
                <button class="biz-pill-btn biz-cinema-btn" :class="{ active: sidePanelCollapsed, disabled: cinemaDisabled }" @click="handleCinemaMode">
                  <svg class="biz-cinema-icon" viewBox="0 0 16 16" aria-hidden="true">
                    <rect x="1.5" y="3" width="13" height="9" rx="1.1" />
                    <path d="M3.5 10.5h3" />
                    <path d="M1 13.5h14" />
                  </svg>
                </button>
                <div class="biz-btn-tooltip">{{ cinemaTooltipText }}</div>
              </div>
              <div class="biz-tooltip-wrap">
                <button class="biz-pill-btn biz-fullscreen-btn" :class="{ disabled: fullscreenDisabled }" @click="handleBizFullscreen">
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
              <img
                class="live-ended-icon-img"
                :src="LiveEndedIcon"
                :alt="t('The host is not currently live')"
              >
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
    <TUIDialog
      v-model:visible="exitLiveDialogVisible"
      :title="t('Exit Live')"
    >
      {{ t('LiveExitConfirmCoGuestTip') }}
      <template #footer>
        <div class="exit-live-action-buttons">
          <TUIButton
            color="blue"
            @click="handleCancelExit"
          >
            {{ t('Cancel') }}
          </TUIButton>
          <TUIButton
            color="red"
            type="primary"
            @click="handleEndCoGuestAndStay"
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
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted, computed, onUnmounted, watch } from 'vue';
import TUIRoomEngine, { TUIAutoPlayCallbackInfo, TUIRoomEvents } from '@tencentcloud/tuiroom-engine-js';
import {
  TUIMessageBox,
  TUIToast,
  TUIButton,
  TUIDialog,
  IconUser,
  useUIKit,
} from '@tencentcloud/uikit-base-component-vue3';
import {
  LiveView,
  useLiveListState,
  useLiveAudienceState,
  useLoginState,
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
import LiveEndedIcon from '../../TUILiveKit/icons/live-ended.svg';

const { t } = useUIKit();

const { audienceList } = useLiveAudienceState();
const { loginUserInfo } = useLoginState();

// Mute detection: show toast when the current user is muted by the host
const localAudience = computed(() => audienceList.value.find(item => item.userId === loginUserInfo.value?.userId));
const isMessageMuted = computed(() => !!localAudience.value?.isMessageDisabled);
watch(isMessageMuted, (newVal, oldVal) => {
  if (newVal && !oldVal) {
    TUIToast.info({ message: t('You have been muted in this room') });
  }
  if (!newVal && oldVal) {
    TUIToast.info({ message: t('You have been unmuted in this room') });
  }
});

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
  if (playToggleDisabled.value) {
    TUIToast.warning({ message: t('LiveView.NotAllowPauseInPIP') });
    return;
  }
  if (isPlaying.value) {
    clearAutoPlayPromptState();
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
  if (pipDisabled.value) {
    if (!isPlaying.value) {
      TUIToast.warning({ message: t('LiveView.NotAllowPIPInNonPlaying') });
      return;
    }
    if (bizIsFullscreen.value) {
      TUIToast.warning({ message: t('LiveView.NotAllowPIPInFullscreen') });
      return;
    }
  }
  if (isPictureInPicture.value) {
    sdkExitPiP();
  } else {
    sdkRequestPiP();
  }
};

const handleBizRefresh = async () => {
  if (isPictureInPicture.value) {
    TUIToast.warning({ message: t('Not allowed to refresh in picture-in-picture mode') });
    return;
  }
  if (isManualRefreshRequesting.value) return;
  isManualRefreshRequesting.value = true;
  manualRefreshCycle.value += 1;
  const currentCycle = manualRefreshCycle.value;
  markManualRefreshPending();
  clearAutoPlayPromptState();
  clearManualRefreshTimer();
  clearRefreshRecoveryTimer();
  isManualRefreshing.value = true;
  manualRefreshTimeoutTimer = setTimeout(() => {
    if (manualRefreshCycle.value === currentCycle) {
      finishManualRefreshing();
    }
  }, 12000);
  try {
    await sdkRefresh();
    startVideoReadyProbe();
    startRefreshRecoveryTimer();
  } catch (error) {
    console.error('Failed to refresh playback:', error);
    finishManualRefreshing();
  } finally {
    isManualRefreshRequesting.value = false;
  }
};

const handleCinemaMode = () => {
  if (cinemaDisabled.value) {
    TUIToast.warning({ message: t('Not allow to enter cinema mode in fullscreen') });
    return;
  }
  sidePanelCollapsed.value = !sidePanelCollapsed.value;
};

// Native fullscreen state for biz-video-card (overrides SDK fullscreen)
const bizIsFullscreen = ref(false);
const playToggleDisabled = computed(() => isPictureInPicture.value);
const pipDisabled = computed(() => !isPlaying.value || bizIsFullscreen.value);
const fullscreenDisabled = computed(() => isPictureInPicture.value);
const refreshDisabled = computed(() => isManualRefreshRequesting.value || isPictureInPicture.value);
const cinemaDisabled = computed(() => bizIsFullscreen.value);
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
  if (fullscreenDisabled.value) {
    TUIToast.warning({ message: t('LiveView.NotAllowFullscreenInPIP') });
    return;
  }
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
 * If currently in picture-in-picture mode, exit PiP first because
 * switching resolution while in PiP is not supported.
 */
const selectResolution = async (opt: { label: string; value: number }) => {
  if (isPictureInPicture.value) {
    await sdkExitPiP();
  }
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
const roomEngine = useRoomEngine();
TUIRoomEngine.once('ready', () => {
  roomEngine.instance?.on(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
});

const props = defineProps<{
  liveId: string;
}>();

const {
  handleApplyForSeat,
  isUserOnSeat,
  confirmLeaveSeat,
  openLeaveSeatDialog,
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

const liveContainerRef = ref<HTMLElement | null>(null);
const bizVideoCardRef = ref<HTMLElement | null>(null);
const liveEndedOverlayVisible = ref(false);
const autoPlayFailedHandled = ref(false);
const autoPlayPromptVisible = ref(false);
const isManualRefreshing = ref(false);
const isManualRefreshRequesting = ref(false);
const manualRefreshCycle = ref(0);
const readyEmitted = ref(false);
let videoReadyProbeTimer: ReturnType<typeof setInterval> | null = null;
let observedVideoEl: HTMLVideoElement | null = null;
let autoPlayResumeAction: (() => void) | null = null;
let manualRefreshTimeoutTimer: ReturnType<typeof setTimeout> | null = null;
let refreshRecoveryTimer: ReturnType<typeof setTimeout> | null = null;
let manualRefreshPendingFrame = false;
let manualRefreshStartMediaTime = -1;
let manualRefreshPlaybackSignal = false;
let manualRefreshFrameCallbackId: number | null = null;

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
    }
  }, READY_SAFETY_TIMEOUT_MS);
}

function showAutoPlayPrompt(resumeAction: () => void) {
  if (autoPlayFailedHandled.value || liveEndedOverlayVisible.value) return;
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
  startVideoReadyProbe();
  resume?.();
}

function clearManualRefreshTimer() {
  if (manualRefreshTimeoutTimer) {
    clearTimeout(manualRefreshTimeoutTimer);
    manualRefreshTimeoutTimer = null;
  }
}

function clearManualRefreshFrameCallback() {
  if (!observedVideoEl || manualRefreshFrameCallbackId === null || typeof observedVideoEl.cancelVideoFrameCallback !== 'function') {
    manualRefreshFrameCallbackId = null;
    return;
  }
  observedVideoEl.cancelVideoFrameCallback(manualRefreshFrameCallbackId);
  manualRefreshFrameCallbackId = null;
}

function queueManualRefreshFrameCallback(videoEl: HTMLVideoElement) {
  if (!manualRefreshPendingFrame || typeof videoEl.requestVideoFrameCallback !== 'function' || manualRefreshFrameCallbackId !== null) {
    return;
  }
  const currentVideo = videoEl;
  manualRefreshFrameCallbackId = videoEl.requestVideoFrameCallback(() => {
    manualRefreshFrameCallbackId = null;
    if (!manualRefreshPendingFrame || observedVideoEl !== currentVideo) {
      return;
    }
    manualRefreshPlaybackSignal = true;
    if (isManualRefreshing.value && hasManualRefreshRendered(currentVideo)) {
      finishManualRefreshing();
    }
  });
}

function markManualRefreshPending() {
  manualRefreshPendingFrame = true;
  manualRefreshStartMediaTime = observedVideoEl?.currentTime ?? -1;
  manualRefreshPlaybackSignal = false;
  clearManualRefreshFrameCallback();
  if (observedVideoEl) {
    queueManualRefreshFrameCallback(observedVideoEl);
  }
}

function hasManualRefreshRendered(videoEl: HTMLVideoElement): boolean {
  if (!manualRefreshPendingFrame) {
    return true;
  }
  if (!isVideoReady(videoEl) || videoEl.paused || videoEl.ended) {
    return false;
  }
  if (manualRefreshPlaybackSignal) {
    return true;
  }
  if (manualRefreshStartMediaTime < 0) {
    return videoEl.currentTime > 0;
  }
  return videoEl.currentTime > manualRefreshStartMediaTime + 0.01;
}

const REFRESH_RECOVERY_POLL_MS = 180;

function clearRefreshRecoveryTimer() {
  if (refreshRecoveryTimer) {
    clearTimeout(refreshRecoveryTimer);
    refreshRecoveryTimer = null;
  }
}

function startRefreshRecoveryTimer() {
  clearRefreshRecoveryTimer();
  refreshRecoveryTimer = setTimeout(() => {
    if (liveEndedOverlayVisible.value) {
      finishManualRefreshing();
      return;
    }
    const videoReady = observedVideoEl ? isVideoReady(observedVideoEl) : false;
    const freshFrameRendered = observedVideoEl ? hasManualRefreshRendered(observedVideoEl) : false;

    // Keep loading visible until this refresh really renders a new frame.
    // The 12s manualRefreshTimeoutTimer remains the final fallback to avoid
    // an infinite loading state if the stream never recovers.
    if (!videoReady || !freshFrameRendered) {
      startRefreshRecoveryTimer();
      return;
    }

    finishManualRefreshing();
  }, REFRESH_RECOVERY_POLL_MS);
}

function finishManualRefreshing() {
  isManualRefreshing.value = false;
  manualRefreshPendingFrame = false;
  manualRefreshStartMediaTime = -1;
  manualRefreshPlaybackSignal = false;
  clearManualRefreshFrameCallback();
  clearManualRefreshTimer();
  clearRefreshRecoveryTimer();
}

// Hover show/hide for control overlay
const showBizControls = ref(false);
const isControlOverlayHovered = ref(false);
let bizControlsHideTimer: ReturnType<typeof setTimeout> | null = null;
const CONTROLS_HIDE_DELAY_MS = 3000;

const startBizControlsHideTimer = (delay = CONTROLS_HIDE_DELAY_MS) => {
  cancelBizControlsHideTimer();
  bizControlsHideTimer = setTimeout(() => {
    if (isControlOverlayHovered.value || isVolumeAreaHovered.value || isVolumeInteracting.value) {
      startBizControlsHideTimer(CONTROLS_HIDE_DELAY_MS);
      return;
    }
    showBizControls.value = false;
  }, delay);
};

const cancelBizControlsHideTimer = () => {
  if (bizControlsHideTimer) {
    clearTimeout(bizControlsHideTimer);
    bizControlsHideTimer = null;
  }
};

const handleBizVideoMouseEnter = () => {
  showBizControls.value = true;
  startBizControlsHideTimer(CONTROLS_HIDE_DELAY_MS);
};

const handleBizVideoMouseMove = () => {
  showBizControls.value = true;
  startBizControlsHideTimer(CONTROLS_HIDE_DELAY_MS);
};

const handleBizVideoMouseLeave = () => {
  startBizControlsHideTimer(CONTROLS_HIDE_DELAY_MS);
};

const handleControlOverlayEnter = () => {
  isControlOverlayHovered.value = true;
  showBizControls.value = true;
  cancelBizControlsHideTimer();
};

const handleControlOverlayMove = () => {
  isControlOverlayHovered.value = true;
  showBizControls.value = true;
  cancelBizControlsHideTimer();
};

const handleControlOverlayLeave = () => {
  isControlOverlayHovered.value = false;
  startBizControlsHideTimer(CONTROLS_HIDE_DELAY_MS);
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

function clearAutoPlayPromptState() {
  autoPlayPromptVisible.value = false;
  autoPlayResumeAction = null;
  autoPlayFailedHandled.value = false;
}

function tryEmitReadyFromVideo(event?: Event) {
  const eventType = event?.type;
  if (observedVideoEl && isVideoReady(observedVideoEl)) {
    if (eventType === 'play' || eventType === 'playing' || eventType === 'timeupdate') {
      manualRefreshPlaybackSignal = true;
    } else {
      queueManualRefreshFrameCallback(observedVideoEl);
    }
    if (isManualRefreshing.value && hasManualRefreshRendered(observedVideoEl)) {
      finishManualRefreshing();
    }
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
  videoEl.addEventListener('play', tryEmitReadyFromVideo);
  videoEl.addEventListener('timeupdate', tryEmitReadyFromVideo);
  tryEmitReadyFromVideo();
}

function unbindVideoReadyListeners() {
  if (!observedVideoEl) return;
  clearManualRefreshFrameCallback();
  observedVideoEl.removeEventListener('loadeddata', tryEmitReadyFromVideo);
  observedVideoEl.removeEventListener('canplay', tryEmitReadyFromVideo);
  observedVideoEl.removeEventListener('playing', tryEmitReadyFromVideo);
  observedVideoEl.removeEventListener('play', tryEmitReadyFromVideo);
  observedVideoEl.removeEventListener('timeupdate', tryEmitReadyFromVideo);
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

watch(isUserOnSeat, (onSeat) => {
  if (!onSeat) return;
  if (!sidePanelCollapsed.value) return;
  sidePanelCollapsed.value = false;
});

onMounted(async () => {
  startVideoReadyProbe();
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
  roomEngine.instance?.off(TUIRoomEvents.onAutoPlayFailed, handleAutoPlayFailed);
  cancelBizControlsHideTimer();
  cancelVolumeHideTimer();
  clearReadySafetyTimer();
  clearManualRefreshTimer();
  clearRefreshRecoveryTimer();
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

const exitLiveDialogVisible = ref(false);

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

async function handleEndCoGuestAndStay() {
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

function handleDisconnectCoGuest() {
  openLeaveSeatDialog();
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
  if (autoPlayFailedHandled.value) {
    return;
  }
  emitReadyOnce();
  finishManualRefreshing();
  showAutoPlayPrompt(() => event.resume());
}

watch(isPlaying, (playing) => {
  if (playing) {
    startVideoReadyProbe();
    emitReadyOnce();
    if (isManualRefreshing.value && observedVideoEl && hasManualRefreshRendered(observedVideoEl)) {
      finishManualRefreshing();
    }
  }
}, { immediate: true });

watch(bizIsFullscreen, () => {
  showBizControls.value = true;
  cancelBizControlsHideTimer();
  startBizControlsHideTimer(CONTROLS_HIDE_DELAY_MS);
});

watch(isUserOnSeat, (onSeat) => {
  if (!onSeat) {
    showBizControls.value = true;
    startBizControlsHideTimer(CONTROLS_HIDE_DELAY_MS);
  }
});
</script>

<style lang="scss" scoped>
.live-player-business-pc {
  --biz-stage-edge-spacing: var(--preset-stage-edge-spacing, 16px);
  --biz-stage-edge-spacing-mobile: var(--preset-stage-edge-spacing-mobile, 12px);

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
  display: flex;
  flex-direction: column;
  gap: 8px;
  padding: 8px var(--biz-stage-edge-spacing) 10px;
  box-sizing: border-box;
  background: var(--preset-bg-base);
}

.biz-stage-topbar {
  position: relative;
  height: 54px;
  flex-shrink: 0;
  overflow: visible;
  opacity: 1;
}

.biz-video-top-gradient {
  position: absolute;
  inset: 0;
  border-radius: 12px;
  pointer-events: none;
  background: linear-gradient(
    180deg,
    var(--preset-top-overlay-gradient-start) 0%,
    var(--preset-top-overlay-gradient-mid) 58%,
    var(--preset-top-overlay-gradient-end) 100%
  );
}

.biz-video-top-overlay {
  position: absolute;
  inset: 0;
  z-index: 2;
  display: flex;
  align-items: center;
  justify-content: space-between;
  pointer-events: none;
}

.biz-stage-topbar {
  .stream-top-unified {
    width: 100%;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    padding: 6px 0;
    pointer-events: auto;
    background: transparent;
    border: 0;
    box-shadow: none;
  }

  .stream-meta-glass,
  .stream-actions-glass {
    pointer-events: auto;
    background: transparent;
    border: 0;
    box-shadow: none;
  }

  .stream-meta-glass {
    display: flex;
    align-items: center;
    gap: 12px;
    padding: 0;
    min-width: 0;
    flex: 1;
    max-width: min(620px, 80%);
  }

  .stream-host-avatar-img {
    border-radius: 999px;
    overflow: hidden;
    box-shadow: var(--preset-top-overlay-avatar-shadow);
  }

  .stream-ended-avatar {
    width: 32px;
    height: 32px;
    border-radius: 50%;
    background: rgba(255, 255, 255, 0.12);
    display: flex;
    align-items: center;
    justify-content: center;
    flex-shrink: 0;
    color: rgba(255, 255, 255, 0.55);
  }

  .stream-meta-text {
    min-width: 0;
    display: flex;
    flex-direction: column;
    align-items: flex-start;
    justify-content: center;
    gap: 4px;
  }

  .stream-title-row {
    display: flex;
    align-items: center;
    gap: 12px;
    min-width: 0;
  }

  .stream-live-badge {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    color: var(--preset-live-indicator-text);
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 0.1em;
    line-height: 1;
    flex-shrink: 0;
  }

  .stream-live-badge-dot {
    position: relative;
    width: 7px;
    height: 7px;
    border-radius: 999px;
    background: var(--preset-live-indicator-dot);
    box-shadow: 0 0 10px var(--preset-live-indicator-dot-glow);
    animation: biz-live-pulse 1.5s ease-in-out infinite;

    &::after {
      content: '';
      position: absolute;
      inset: -2px;
      border-radius: 999px;
      border: 1px solid var(--preset-live-indicator-ring);
      animation: biz-live-ring-pulse 2.4s ease-in-out infinite;
    }
  }

  .stream-title {
    color: var(--preset-top-overlay-text-color);
    text-shadow: var(--preset-top-overlay-text-shadow);
    font-size: 14px;
    font-weight: 700;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 100%;
    letter-spacing: 0.01em;
  }

  .stream-actions-glass {
    display: flex;
    align-items: center;
    justify-content: flex-start;
    gap: 0;
    flex-shrink: 0;
    border-radius: 14px;
    overflow: hidden;
    background: rgba(255, 255, 255, 0.04);
    border: 1px solid color-mix(in srgb, var(--preset-top-action-icon-color) 10%, transparent);
  }

  .biz-segment-btn {
    min-width: 112px;
    height: 40px;
    padding: 0 18px;
    border: 0;
    border-radius: 0;
    margin: 0;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    color: var(--preset-top-action-icon-color);
    background: transparent;
    cursor: pointer;
    transition: background-color 180ms ease, color 180ms ease;

    &:hover {
      background: color-mix(in srgb, var(--preset-top-action-icon-hover-bg) 72%, transparent);
      color: var(--preset-top-action-icon-hover-color);
    }

    &:active {
      background: color-mix(in srgb, var(--preset-top-action-icon-hover-bg) 88%, transparent);
    }
  }

  .biz-segment-btn-copy {
    background: rgba(255, 255, 255, 0.1);
    border-right: 1px solid rgba(255, 255, 255, 0.08);
  }

  .biz-segment-btn-disconnect {
    background: rgba(255, 255, 255, 0.08);
    border-right: 1px solid rgba(255, 255, 255, 0.08);
  }

  .biz-segment-btn-leave {
    background: rgba(255, 255, 255, 0.05);
    color: var(--preset-top-action-leave-icon-color);

    &:hover,
    &:active {
      background: rgba(255, 255, 255, 0.05);
      color: var(--preset-top-action-leave-icon-color);
    }
  }

  .biz-segment-label {
    font-size: 14px;
    line-height: 1;
    font-weight: 700;
    letter-spacing: 0.02em;
  }
}

.biz-video-card {
  width: 100%;
  flex: 1;
  min-height: 0;
  position: relative;
  border-radius: 14px;
  overflow: hidden;
  background: var(--preset-video-bg);
  border: 1px solid var(--preset-stage-divider, rgba(255, 255, 255, 0.08));

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

  .stream-top-unified {
    width: 100%;
    margin-left: 0;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    padding: 4px 0;
    border-radius: 0;
    pointer-events: auto;
    background: transparent;
    border: 0;
    backdrop-filter: none;
    box-shadow: none;
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
    gap: 12px;
    padding: 0;
    border-radius: 0;
    min-width: 0;
    flex: 1;
    max-width: min(620px, 80%);
  }

  .stream-host-avatar {
    position: relative;
    flex-shrink: 0;
    display: inline-flex;
    align-items: center;
    justify-content: center;
  }

  .stream-host-avatar-img {
    border-radius: 999px;
    overflow: hidden;
    box-shadow: var(--preset-top-overlay-avatar-shadow);
  }

  .stream-ended-avatar {
    width: 32px;
    height: 32px;
    border-radius: 50%;
    background: rgba(255, 255, 255, 0.12);
    display: flex;
    align-items: center;
    justify-content: center;
    flex-shrink: 0;
    color: rgba(255, 255, 255, 0.55);
  }

  .stream-meta-text {
    min-width: 0;
    display: flex;
    align-items: center;
    justify-content: flex-start;
  }

  .stream-title-row {
    display: flex;
    align-items: center;
    gap: 12px;
    min-width: 0;
  }

  .stream-live-badge {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    padding: 0;
    border-radius: 0;
    background: transparent;
    border: 0;
    color: var(--preset-live-indicator-text);
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 0.1em;
    flex-shrink: 0;
  }

  .stream-live-badge-dot {
    position: relative;
    width: 7px;
    height: 7px;
    border-radius: 999px;
    background: var(--preset-live-indicator-dot);
    box-shadow: 0 0 10px var(--preset-live-indicator-dot-glow);
    animation: biz-live-pulse 1.5s ease-in-out infinite;

    &::after {
      content: '';
      position: absolute;
      inset: -2px;
      border-radius: 999px;
      border: 1px solid var(--preset-live-indicator-ring);
      animation: biz-live-ring-pulse 2.4s ease-in-out infinite;
    }
  }

  .stream-title {
    color: var(--preset-top-overlay-text-color);
    text-shadow: var(--preset-top-overlay-text-shadow);
    font-size: 14px;
    font-weight: 700;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    max-width: 100%;
    letter-spacing: 0.01em;
  }

  .stream-actions-glass {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 12px;
    border-radius: 0;
    padding: 0;
    border-left: 0;
    flex-shrink: 0;
  }

  .top-action-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 42px;
    height: 42px;
    border: 0;
    border-radius: 999px;
    padding: 0;
    color: var(--preset-top-action-icon-color);
    background: var(--preset-top-action-icon-bg);
    cursor: pointer;
    transition: transform 180ms ease, background-color 180ms ease, color 180ms ease;

    &:hover {
      color: var(--preset-top-action-icon-hover-color);
      background: var(--preset-top-action-icon-hover-bg);
      transform: scale(1.04);
    }

    &:active {
      transform: scale(0.96);
    }

    svg {
      opacity: 1;
      width: 20px;
      height: 20px;
    }
  }

  .top-action-btn-leave {
    color: var(--preset-top-action-leave-icon-color);
    background: var(--preset-top-action-leave-icon-bg);
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

  &.controls-visible-bottom .biz-control-overlay {
    opacity: 1;
    transform: translateY(0);
    z-index: 40;

    > * {
      pointer-events: auto;
    }
  }

  @keyframes biz-live-pulse {
    0% {
      transform: scale(0.9);
      opacity: 0.78;
    }
    50% {
      transform: scale(1);
      opacity: 1;
    }
    100% {
      transform: scale(0.9);
      opacity: 0.78;
    }
  }

  @keyframes biz-live-ring-pulse {
    0% {
      opacity: 0.85;
      transform: scale(0.92);
    }
    50% {
      opacity: 0.45;
      transform: scale(1);
    }
    100% {
      opacity: 0.85;
      transform: scale(0.92);
    }
  }

  .biz-progress-bar {
    position: absolute;
    bottom: 76px;
    left: var(--biz-stage-edge-spacing);
    right: var(--biz-stage-edge-spacing);
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
    padding: 0 var(--biz-stage-edge-spacing) 20px;
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

    &.disabled {
      opacity: 0.46;
      cursor: not-allowed;

      &:hover {
        background: transparent;
      }

      &:active {
        transform: none;
      }
    }
  }

  .biz-refresh-btn {
    &.disabled {
      opacity: 0.45;
      cursor: not-allowed;
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

  .biz-top-action-wrap {
    .biz-btn-tooltip {
      top: calc(100% + 8px);
      bottom: auto;
      transform: translateX(-50%) translateY(-4px);
    }

    &:hover .biz-btn-tooltip {
      transform: translateX(-50%) translateY(0);
    }

    &.tooltip-hidden .biz-btn-tooltip {
      transform: translateX(-50%) translateY(-4px) !important;
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
      background: var(--preset-control-btn-hover-bg, rgba(255, 255, 255, 0.1));
      box-shadow: none;
    }

    &.active:hover {
      background: var(--preset-control-btn-hover-bg, rgba(255, 255, 255, 0.1));
    }

    .biz-cinema-icon {
      width: 24px;
      height: 24px;
      fill: none !important;
      stroke: var(--preset-control-btn-color, #ffffff);
      stroke-width: 1.5;
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
      width: auto;
      height: auto;
      display: flex;
      align-items: center;
      justify-content: center;
      margin-bottom: 4px;
    }

    .live-ended-icon-img {
      width: 58px;
      height: 60px;
      opacity: 0.96;
      filter: drop-shadow(0 6px 14px rgba(15, 23, 42, 0.55));
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
    --biz-stage-edge-spacing: var(--biz-stage-edge-spacing-mobile);
    flex-direction: column;
  }

  .biz-video-card {
    border-right: 0;
    border-radius: 12px;
  }

  .biz-left-column {
    padding: 8px var(--biz-stage-edge-spacing) 8px;
  }

  .biz-stage-topbar {
    height: 50px;

    .stream-top-unified {
      width: 100%;
      padding: 2px 0;
      gap: 8px;
    }

    .stream-meta-glass {
      max-width: none;
    }

    .stream-actions-glass {
      gap: 6px;
      padding-left: 4px;
    }
  }

  .biz-panel-wrapper {
    width: 100%;
  }

}

.exit-live-action-buttons {
  display: flex;
  gap: 10px;
}
</style>
