<template>
  <!-- Custom player controls: always visible, Douyin-style split bar.
       Left cluster: play/pause + refresh. Right cluster: resolution +
       picture-in-picture + volume + fullscreen. Icons come from the base
       icon library (@tencentcloud/uikit-base-component-vue3).
       Every button shows a floating tooltip bubble on hover (Douyin-style).
       The resolution button reveals its option list via hover, no click needed. -->
  <div class="player-controls-overlay">
    <!-- Left cluster: play/pause + refresh -->
    <div class="pc-cluster pc-left">
      <!-- Play / Pause -->
      <div class="pc-tooltip-btn" :data-tip="t(isPlaying ? 'Pause' : 'Play')">
        <button class="pc-btn" @click.stop="togglePlayPause">
          <IconBusinessPause v-if="isPlaying" :size="18" />
          <IconBusinessPlay v-else :size="18" />
        </button>
      </div>

      <!-- Refresh -->
      <div class="pc-tooltip-btn" :data-tip="t('Refresh')">
        <button class="pc-btn" :disabled="isRefreshing" @click.stop="handleRefresh">
          <IconRefresh :size="18" />
        </button>
      </div>
    </div>

    <!-- Right cluster: resolution + picture-in-picture + volume + fullscreen -->
    <div class="pc-cluster pc-right">
      <!-- Resolution selector: hover to reveal options above -->
      <div
        class="pc-resolution-wrap"
        @mouseenter="resolutionMenuOpen = true"
        @mouseleave="resolutionMenuOpen = false"
      >
        <div class="pc-tooltip-btn pc-res-trigger" :data-tip="t('Resolution')">
          <button class="pc-btn pc-btn-resolution" :class="{ active: resolutionMenuOpen }">
            {{ currentResolutionLabel }}
          </button>
        </div>
        <!-- Hover popover: resolution options displayed above the button -->
        <div v-if="resolutionMenuOpen" class="pc-res-menu">
          <div
            v-for="res in resolutionItems"
            :key="res.value"
            class="pc-res-item"
            :class="{ active: isActiveResolution(res) }"
            @click.stop="handleSwitchResolution(res)"
          >
            {{ res.label }}
          </div>
        </div>
      </div>

      <!-- Picture-in-Picture -->
      <div class="pc-tooltip-btn" :data-tip="t(isPictureInPicture ? 'ExitPiP' : 'EnterPiP')">
        <button class="pc-btn" @click.stop="togglePictureInPicture">
          <IconBusinessPictureInPicture :size="18" />
        </button>
      </div>

      <!-- Volume: hover reveals a vertical slider; click the icon to mute/unmute -->
      <div
        class="pc-volume-wrap"
        @mouseenter="volumeMenuOpen = true"
        @mouseleave="volumeMenuOpen = false"
      >
        <!-- Hover popover: vertical volume slider displayed above the button -->
        <div
          v-if="volumeMenuOpen"
          class="pc-volume-panel"
          @pointerdown="onVolumePointerDown"
          @pointermove="onVolumePointerMove"
          @pointerup="onVolumePointerUp"
        >
          <span class="pc-volume-value">{{ volumePercent }}</span>
          <div ref="volumeTrackRef" class="pc-volume-track">
            <div class="pc-volume-fill" :style="{ height: `${volumePercent}%` }">
              <span class="pc-volume-thumb" />
            </div>
          </div>
        </div>
        <button class="pc-btn" @click.stop="toggleMute">
          <IconSpeakerOff v-if="showMutedIcon" :size="18" />
          <IconBusinessSound v-else :size="18" />
        </button>
      </div>

      <!-- Fullscreen -->
      <div class="pc-tooltip-btn" :data-tip="t(isFullscreen ? 'ExitFullscreen' : 'Fullscreen')">
        <button class="pc-btn" @click.stop="toggleFullscreen">
          <IconBusinessFullscreenExit v-if="isFullscreen" :size="18" />
          <IconBusinessFullscreen v-else :size="18" />
        </button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue';
import {
  useUIKit,
  IconBusinessPlay,
  IconBusinessPause,
  IconBusinessFullscreen,
  IconBusinessFullscreenExit,
  IconBusinessPictureInPicture,
  IconBusinessSound,
  IconSpeakerOff,
  IconRefresh,
} from '@tencentcloud/uikit-base-component-vue3';
import { useLivePlayerState } from 'tuikit-atomicx-vue3';
import { setFullScreen, exitFullScreen } from '../../utils/utils';

const { t } = useUIKit();

// ── Custom player controls state ──────────────────────────────────────
const {
  isPlaying,
  isMuted,
  isPictureInPicture,
  currentVolume,
  currentResolution,
  resolutionList,
  pause,
  resume,
  refresh,
  mute,
  unmute,
  requestPictureInPicture,
  exitPictureInPicture,
  switchResolution,
  setVolume,
} = useLivePlayerState();

// Local fullscreen state driven by the browser-native Fullscreen API (NOT the
// SDK). Tracks whether the whole player page (#liveContainer / .live-player-pc,
// i.e. the immersive left area + right chat rail) is currently fullscreen, so
// the control icon can toggle correctly.
const isFullscreen = ref(false);

function syncFullscreenState() {
  const liveContainer = document.getElementById('liveContainer');
  isFullscreen.value = !!document.fullscreenElement && document.fullscreenElement === liveContainer;
}

const fullscreenEvents = ['fullscreenchange', 'webkitfullscreenchange', 'mozfullscreenchange', 'msfullscreenchange'];

onMounted(() => {
  fullscreenEvents.forEach((evt) => window.addEventListener(evt, syncFullscreenState));
  syncFullscreenState();
});

onUnmounted(() => {
  fullscreenEvents.forEach((evt) => window.removeEventListener(evt, syncFullscreenState));
});

// Normalized resolution shape so the template never reaches into the raw
// (loosely typed) refs returned by the state hook.
type ResolutionItem = { label: string; value: number };

const resolutionMenuOpen = ref(false);
const isRefreshing = ref(false);

// ── Volume slider state ───────────────────────────────────────────────
const volumeMenuOpen = ref(false);
const volumeTrackRef = ref<HTMLElement>();
const isDraggingVolume = ref(false);

// Volume is a 0-100 integer; drives the vertical fill height directly.
const volumePercent = computed(() => Math.round(currentVolume.value));

// Map a pointer position to a 0-100 volume value based on the vertical
// track height. Bottom edge = 0, top edge = 100. Works for both clicking
// the track and dragging the thumb (thumb is visually nested but
// pointer-events are disabled so the event reaches the track).
function applyVolumeFromEvent(e: PointerEvent) {
  const el = volumeTrackRef.value;
  if (!el) return;
  const rect = el.getBoundingClientRect();
  const ratio = (rect.bottom - e.clientY) / rect.height;
  const clamped = Math.min(1, Math.max(0, ratio));
  const volume = Math.round(clamped * 100);
  setVolume(volume).catch(() => {});
}

function onVolumePointerDown(e: PointerEvent) {
  isDraggingVolume.value = true;
  try { (e.currentTarget as HTMLElement).setPointerCapture(e.pointerId); } catch {}
  applyVolumeFromEvent(e);
}

function onVolumePointerMove(e: PointerEvent) {
  if (!isDraggingVolume.value) return;
  applyVolumeFromEvent(e);
}

function onVolumePointerUp(e: PointerEvent) {
  isDraggingVolume.value = false;
  try { (e.currentTarget as HTMLElement).releasePointerCapture(e.pointerId); } catch {}
}

// Coerce the hook's resolution list into a stable, typed shape. The runtime
// value is always an array of { label, value }; the cast keeps template type
// checking strict without depending on the hook's exported generics.
const resolutionItems = computed<ResolutionItem[]>(
  () => resolutionList.value as unknown as ResolutionItem[],
);

const currentResolutionItem = computed<ResolutionItem | undefined>(
  () => currentResolution.value as unknown as ResolutionItem | undefined,
);

const currentResolutionLabel = computed(() => {
  if (currentResolutionItem.value?.label) return currentResolutionItem.value.label;
  // Fallback to the first resolution label
  if (resolutionItems.value.length > 0) return resolutionItems.value[0].label;
  // Last-resort fallback — resolved via i18n so it follows the active locale.
  return t('Standard Definition');
});

function isActiveResolution(res: ResolutionItem): boolean {
  return currentResolutionItem.value?.value === res.value;
}

// Mirror of `isMuted || currentVolume === 0` for the muted-icon toggle,
// kept here so the template only reads a plain boolean.
const showMutedIcon = computed(() => isMuted.value || currentVolume.value === 0);

function handleSwitchResolution(res: ResolutionItem) {
  try {
    switchResolution(res as Parameters<typeof switchResolution>[0]);
  } catch {}
  // Close on selection so the popover doesn't linger
  resolutionMenuOpen.value = false;
}

async function togglePictureInPicture() {
  try {
    if (isPictureInPicture.value) await exitPictureInPicture(); else await requestPictureInPicture();
  } catch {}
}

async function toggleMute() {
  try {
    if (isMuted.value || currentVolume.value === 0) await unmute();
    else await mute();
  } catch {}
}

// Use the browser-native Fullscreen API to make the WHOLE player page
// (#liveContainer / .live-player-pc) fullscreen — not the SDK's element-only
// fullscreen. This mirrors FullScreen.vue's behavior.
function toggleFullscreen() {
  const liveContainer = document.getElementById('liveContainer');
  if (!liveContainer) return;
  if (isFullscreen.value) {
    exitFullScreen();
  } else {
    setFullScreen(liveContainer, { navigationUI: 'hide' });
  }
}

async function togglePlayPause() {
  try {
    if (isPlaying.value) await pause(); else await resume();
  } catch {}
}

async function handleRefresh() {
  if (isRefreshing.value) return;
  isRefreshing.value = true;
  try {
    await refresh();
  } catch {}
  finally {
    isRefreshing.value = false;
  }
}
</script>

<style lang="scss" scoped>
.player-controls-overlay {
  position: absolute;
  left: 12px;
  right: 12px;
  bottom: 10px;
  z-index: 15;
  display: flex;
  align-items: center;
  justify-content: space-between;
  // Let clicks pass through the empty middle to the video underneath;
  // only the button clusters capture pointer events.
  pointer-events: none;

  .pc-cluster {
    display: flex;
    align-items: center;
    gap: 4px;
    pointer-events: auto;
  }

  // ── Tooltip button wrapper (Douyin-style hover bubble) ───────────
  // Each .pc-tooltip-btn shows a dark rounded pill above its child button
  // on hover, using the data-tip attribute as text content.
  .pc-tooltip-btn {
    position: relative;
    display: inline-flex;

    &::after {
      content: attr(data-tip);
      position: absolute;
      bottom: 100%;
      left: 50%;
      transform: translateX(-50%) translateY(-4px);
      padding: 5px 10px;
      font-size: 12px;
      line-height: 1.3;
      font-weight: 500;
      color: #fff;
      background: rgba(30, 30, 38, 0.9);
      backdrop-filter: blur(16px);
      -webkit-backdrop-filter: blur(16px);
      border-radius: 6px;
      white-space: nowrap;
      pointer-events: none;
      opacity: 0;
      transition: opacity 0.15s ease, transform 0.15s ease;
      box-shadow: 0 4px 14px rgba(0, 0, 0, 0.35);
      z-index: 25;
    }

    // Small downward triangle pointing to the button
    &::before {
      content: '';
      position: absolute;
      bottom: 100%;
      left: 50%;
      transform: translateX(-50%) translateY(-2px);
      border: 5px solid transparent;
      border-top-color: rgba(30, 30, 38, 0.9);
      pointer-events: none;
      opacity: 0;
      transition: opacity 0.15s ease;
      z-index: 25;
    }

    &:hover::after,
    &:hover::before {
      opacity: 1;
    }
  }

  // Wrapper keeps the resolution popover anchored to its trigger.
  .pc-resolution-wrap {
    position: relative;
    display: inline-flex;

    // When hovering the wrap, suppress the default tooltip so only the
    // resolution list popover is visible (avoids overlapping bubbles).
    &:hover > .pc-tooltip-btn::after,
    &:hover > .pc-tooltip-btn::before {
      opacity: 0 !important;
    }
  }

  // Wrapper keeps the volume slider popover anchored to its trigger button.
  .pc-volume-wrap {
    position: relative;
    display: inline-flex;
    pointer-events: auto;
  }

  // Volume hover popover — vertical slider matching Douyin style.
  .pc-volume-panel {
    position: absolute;
    left: 50%;
    transform: translateX(-50%);
    bottom: 36px;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 10px;
    padding: 14px 14px 16px;
    background: rgba(40, 40, 50, 0.92);
    backdrop-filter: blur(20px);
    -webkit-backdrop-filter: blur(20px);
    border-radius: 10px;
    box-shadow: 0 6px 20px rgba(0, 0, 0, 0.4);
    cursor: pointer;
    z-index: 20;

    // Invisible bridge filling the gap down to the button so moving the
    // cursor from the button up into the panel does not trigger mouseleave.
    &::after {
      content: '';
      position: absolute;
      left: 0;
      right: 0;
      bottom: -8px;
      height: 8px;
      background: transparent;
    }

    .pc-volume-value {
      font-size: 13px;
      font-weight: 500;
      color: rgba(255, 255, 255, 0.7);
      line-height: 1;
      min-width: 22px;
      text-align: center;
    }

    // Thin visual track — purely decorative now; the parent panel owns the
    // pointer interaction so clicks anywhere inside the panel set volume.
    // The thumb is also pointer-events disabled so presses on it pass through.
    .pc-volume-track {
      position: relative;
      width: 4px;
      height: 96px;
      border-radius: 2px;
      background: rgba(255, 255, 255, 0.18);
      touch-action: none;

      .pc-volume-fill {
        position: absolute;
        left: 0;
        bottom: 0;
        width: 100%;
        border-radius: 2px;
        // Douyin accent red for the active fill
        background: #FE2C55;

        .pc-volume-thumb {
          position: absolute;
          top: 0;
          left: 50%;
          transform: translate(-50%, -50%);
          width: 14px;
          height: 14px;
          border-radius: 50%;
          background: #fff;
          box-shadow: 0 1px 6px rgba(0, 0, 0, 0.35);
          pointer-events: none;
        }
      }
    }
  }

  .pc-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    min-width: 32px;
    height: 28px;
    padding: 0 8px;
    font-size: 12px;
    font-weight: 600;
    color: rgb(255, 255, 255);
    background: transparent;
    border: none;
    border-radius: 6px;
    cursor: pointer;
    white-space: nowrap;

    &:disabled {
      opacity: 0.5;
      cursor: default;
    }

    svg { flex-shrink: 0; }

    &.pc-btn-resolution {
      // Plain text label — no dropdown chevron needed since the list
      // appears on hover above this button.
    }

    &.active {
      color: #fff;
    }
  }

  // Resolution hover popover (Douyin-style rounded pill)
  .pc-res-menu {
    position: absolute;
    left: 50%;
    transform: translateX(-50%);
    bottom: 32px;
    min-width: 64px;
    padding: 8px 7px;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 4px;
    // Dark semi-transparent pill matching the video overlay aesthetic
    background: rgba(40, 40, 50, 0.92);
    backdrop-filter: blur(20px);
    -webkit-backdrop-filter: blur(20px);
    border-radius: 14px;
    box-shadow: 0 6px 20px rgba(0, 0, 0, 0.4);
    z-index: 20;

    // Invisible bridge filling the gap down to the button so moving the
    // cursor from the button up into the list does not trigger mouseleave.
    &::after {
      content: '';
      position: absolute;
      left: 0;
      right: 0;
      bottom: -8px;
      height: 8px;
      background: transparent;
    }

    .pc-res-item {
      display: inline-flex;       // size-to-content — background hugs the text
      align-items: center;
      justify-content: center;
      gap: 6px;
      padding: 5px 12px;
      font-size: 13px;
      font-weight: 500;
      color: rgba(255, 255, 255, 0.65);
      cursor: pointer;
      border-radius: 8px;
      transition: color 0.12s ease;

      &:hover {
        color: #fff;
      }

      // Selected item — tight inline highlight like Douyin "超清"
      &.active {
        color: #fff;
      }
    }
  }
}
</style>
