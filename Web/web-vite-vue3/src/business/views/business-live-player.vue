<template>
  <div class="business-live-player-container style-preset-business">
    <div class="biz-player-shell" :class="{ hidden: showPageLoading }">
      <LivePlayerBusinessPC
        v-if="loginUserInfo"
        :live-id="liveId as string"
        @leave-live="leaveLive"
        @ready="handlePlayerReady"
      />
    </div>
    <Transition name="biz-page-loading-fade">
      <div v-if="showPageLoading" class="biz-page-loading">
        <div class="biz-page-loading-content">
          <div class="biz-loading-spinner">
            <svg class="biz-loading-orbit" viewBox="0 0 128 128" aria-hidden="true">
              <circle class="orbit-track" cx="64" cy="64" r="50" />
              <g class="orbit-segments">
                <circle class="orbit-segment" cx="64" cy="64" r="50" />
                <circle class="orbit-segment orbit-segment-alt" cx="64" cy="64" r="50" />
              </g>
            </svg>
            <img class="biz-loading-logo" src="../../assets/imgs/logo.svg" alt="logo" />
          </div>
          <p>{{ t('Loading ...') }}</p>
        </div>
      </div>
    </Transition>
  </div>
</template>

<script lang="ts" setup>
import { ref, onMounted, onUnmounted } from 'vue';
import { useRouter, useRoute } from 'vue-router';
import { useLoginState } from 'tuikit-atomicx-vue3';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import LivePlayerBusinessPC from '../components/LivePlayerBusinessPC.vue';
import { useBusinessPreset } from '../composables/useBusinessPreset';
import '../styles/business.scss';
import '../../TUILiveKit';

const { loginUserInfo, login } = useLoginState();
const { isBusinessPreset } = useBusinessPreset();
const { t } = useUIKit();

const router = useRouter();
const route = useRoute();
const { liveId } = route.query;
const showPageLoading = ref(true);
let loadingMinTimer: ReturnType<typeof setTimeout> | null = null;
let loadingTimeoutTimer: ReturnType<typeof setTimeout> | null = null;
let loadingStartTime = 0;

/**
 * Maximum time (ms) the loading overlay stays visible before auto-dismissing.
 * This prevents the loading from getting permanently stuck when the page is
 * refreshed and the autoplay-failed event fires before the listener is bound.
 */
const LOADING_TIMEOUT_MS = 8000;

function dismissLoading() {
  const elapsed = Date.now() - loadingStartTime;
  const remaining = Math.max(0, 900 - elapsed);
  if (loadingMinTimer) {
    clearTimeout(loadingMinTimer);
  }
  loadingMinTimer = setTimeout(() => {
    showPageLoading.value = false;
    loadingMinTimer = null;
  }, remaining);
  clearLoadingTimeout();
}

function handlePlayerReady() {
  dismissLoading();
}

function clearLoadingTimeout() {
  if (loadingTimeoutTimer) {
    clearTimeout(loadingTimeoutTimer);
    loadingTimeoutTimer = null;
  }
}

function leaveLive() {
  const query: Record<string, string> = {};
  if (!isBusinessPreset.value) {
    query.stylePreset = 'business';
  }
  router.push({ path: '/live-list', query });
}

/**
 * Restore login state from sessionStorage.  On a full page refresh the
 * reactive `loginUserInfo` is empty because no component has called
 * `login()` yet.  The standard live-player relies on LiveHeader to do
 * this, but the business variant has no LiveHeader, so we must do it
 * ourselves.
 */
async function restoreLoginState() {
  if (loginUserInfo.value && loginUserInfo.value.userId) {
    return;
  }
  try {
    const storedData = sessionStorage.getItem('tuiLive-userInfo') || '{}';
    const liveUserInfo = JSON.parse(storedData);
    if (liveUserInfo.userID) {
      await login({
        userId: liveUserInfo.userID,
        userSig: liveUserInfo.userSig,
        sdkAppId: liveUserInfo.SDKAppID,
        testEnv: localStorage.getItem('tuikit-live-env') === 'TestEnv',
      });
    }
  } catch (error) {
    console.error('[business-live-player] Failed to restore login state:', error);
    // If login restoration fails, redirect to login page
    router.push({ path: '/login', query: { from: route.path, ...route.query } });
  }
}

onMounted(async () => {
  loadingStartTime = Date.now();
  showPageLoading.value = true;

  // Restore login state on page refresh (same logic as LiveHeader)
  await restoreLoginState();

  // Safety timeout: if the player never emits 'ready' (e.g. after a page
  // refresh where autoplay is blocked), auto-dismiss the loading overlay.
  loadingTimeoutTimer = setTimeout(() => {
    if (showPageLoading.value) {
      dismissLoading();
    }
  }, LOADING_TIMEOUT_MS);
});

onUnmounted(() => {
  if (loadingMinTimer) {
    clearTimeout(loadingMinTimer);
    loadingMinTimer = null;
  }
  clearLoadingTimeout();
});
</script>

<style lang="scss" scoped>
.business-live-player-container {
  position: relative;
  display: flex;
  flex-direction: column;
  width: 100vw;
  height: 100vh;
  padding: 0;
  background-color: var(--preset-bg-base);
  overflow: hidden;
  box-sizing: border-box;
}

.biz-player-shell {
  width: 100%;
  height: 100%;

  &.hidden {
    visibility: hidden;
  }
}

.biz-page-loading {
  position: absolute;
  inset: 0;
  z-index: 90;
  display: flex;
  align-items: center;
  justify-content: center;
  background:
    radial-gradient(circle at 22% 18%, rgba(37, 99, 235, 0.22), transparent 46%),
    radial-gradient(circle at 80% 84%, rgba(14, 165, 233, 0.18), transparent 42%),
    rgba(6, 11, 22, 1);
  backdrop-filter: blur(8px);
}

.biz-page-loading-content {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 16px;
  color: var(--preset-text-primary, #e2e8f0);
  font-size: 15px;
  font-weight: 560;
  letter-spacing: 0.04em;
  font-family: 'SF Pro Display', 'Inter', 'PingFang SC', 'Segoe UI', sans-serif;
  text-rendering: geometricPrecision;

  p {
    margin: 0;
    line-height: 1.25;
    color: color-mix(in srgb, var(--preset-text-primary, #e2e8f0) 88%, transparent);
  }
}

.biz-loading-spinner {
  position: relative;
  width: 132px;
  height: 132px;
  display: flex;
  align-items: center;
  justify-content: center;
}

.biz-loading-orbit {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
}

.orbit-track {
  fill: none;
  stroke: rgba(255, 255, 255, 0.14);
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
  animation: bizOrbitSpin 1.8s infinite;
  will-change: transform;
}

.biz-loading-logo {
  width: 34px;
  height: 34px;
  object-fit: contain;
}

.biz-page-loading-fade-enter-active,
.biz-page-loading-fade-leave-active {
  transition: opacity 220ms ease;
}

.biz-page-loading-fade-enter-from,
.biz-page-loading-fade-leave-to {
  opacity: 0;
}

@keyframes bizSpin {
  to {
    transform: rotate(360deg);
  }
}

@keyframes bizOrbitSpin {
  0% {
    transform: rotate(0deg);
    animation-timing-function: cubic-bezier(0.42, 0, 0.58, 1);
  }

  50% {
    transform: rotate(180deg);
    animation-timing-function: cubic-bezier(0.42, 0, 0.58, 1);
  }

  to {
    transform: rotate(360deg);
  }
}
</style>
