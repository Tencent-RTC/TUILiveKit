<template>
  <div class="education-live-player-container style-preset-education">
    <div class="edu-player-shell" :class="{ hidden: showPageLoading }">
      <LivePlayerEducationPC
        v-if="loginUserInfo"
        :live-id="liveId as string"
        @leave-live="leaveLive"
        @ready="handlePlayerReady"
      />
    </div>
    <Transition name="edu-page-loading-fade">
      <div v-if="showPageLoading" class="edu-page-loading">
        <div class="edu-page-loading-content">
          <div class="edu-loading-stage" aria-hidden="true">
            <div class="edu-loading-glow" />
            <div class="edu-loading-card edu-loading-card-back" />
            <div class="edu-loading-card edu-loading-card-mid" />
            <div class="edu-loading-card edu-loading-card-front">
              <div class="edu-loading-logo-shell">
                <img class="edu-loading-logo" src="../../../assets/imgs/logo.svg" alt="logo" />
              </div>
              <div class="edu-loading-lines">
                <span class="edu-loading-line edu-loading-line-primary" />
                <span class="edu-loading-line edu-loading-line-secondary" />
                <span class="edu-loading-line edu-loading-line-tertiary" />
              </div>
              <div class="edu-loading-progress">
                <span class="edu-loading-progress-bar" />
              </div>
            </div>
          </div>
          <p>{{ t('Loading ...') }}</p>
        </div>
      </div>
    </Transition>
  </div>
</template>

<script lang="ts" setup>
import { ref, onMounted, onUnmounted, nextTick } from 'vue';
import { useRouter, useRoute } from 'vue-router';
import { useLoginState } from 'tuikit-atomicx-vue3';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import LivePlayerEducationPC from '../components/LivePlayerEducationPC.vue';
import { useEducationPreset } from '../composables/useEducationPreset';
import '../styles/education.scss';
import '../../../TUILiveKit';

const { loginUserInfo, login } = useLoginState();
const { isEducationPreset } = useEducationPreset();
const { t } = useUIKit();

const router = useRouter();
const route = useRoute();
const { liveId } = route.query;
const showPageLoading = ref(true);
let loadingMinTimer: ReturnType<typeof setTimeout> | null = null;
let loadingTimeoutTimer: ReturnType<typeof setTimeout> | null = null;
let loadingStartTime = 0;

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
  if (!isEducationPreset.value) {
    query.stylePreset = 'education';
  }
  router.push({ path: '/live-list', query });
}

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
    console.error('[education-live-player] Failed to restore login state:', error);
    router.push({ path: '/login', query: { from: route.path, ...route.query } });
  }
}

onMounted(async () => {
  loadingStartTime = Date.now();
  showPageLoading.value = true;
  await restoreLoginState();

  // Page loading only handles route/login initialization.
  // Stream readiness is handled by the player surface itself.
  await nextTick();
  dismissLoading();

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
.education-live-player-container {
  position: relative;
  display: flex;
  flex-direction: column;
  width: 100vw;
  height: 100vh;
  padding: 0;
  background-color: var(--edu-bg-page);
  overflow: hidden;
  box-sizing: border-box;
}

.edu-player-shell {
  width: 100%;
  height: 100%;

  &.hidden {
    visibility: hidden;
  }
}

.edu-page-loading {
  position: absolute;
  inset: 0;
  z-index: 90;
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--edu-overlay-bg);
  backdrop-filter: blur(8px);
}

.edu-page-loading-content {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 18px;
  color: var(--edu-text-primary);
  font-size: 15px;
  font-weight: 560;
  letter-spacing: 0.04em;
  font-family: 'Noto Sans SC', 'PingFang SC', 'Segoe UI', sans-serif;

  p {
    margin: 0;
    line-height: 1.25;
    color: color-mix(in srgb, var(--edu-text-primary) 88%, transparent);
  }
}

.edu-loading-stage {
  position: relative;
  width: 220px;
  height: 188px;
  display: flex;
  align-items: center;
  justify-content: center;
}

.edu-loading-glow {
  position: absolute;
  left: 50%;
  top: 50%;
  width: 168px;
  height: 168px;
  border-radius: 50%;
  background:
    radial-gradient(circle, color-mix(in srgb, var(--edu-primary) 22%, transparent) 0%, transparent 68%);
  transform: translate(-50%, -50%);
  animation: eduLoadingGlow 2.6s ease-in-out infinite;
}

.edu-loading-card {
  position: absolute;
  left: 50%;
  top: 50%;
  width: 152px;
  height: 124px;
  border-radius: 24px;
  border: 1px solid color-mix(in srgb, var(--edu-border-strong) 72%, transparent);
  background: color-mix(in srgb, var(--edu-bg-surface) 92%, white);
  box-shadow: 0 22px 60px color-mix(in srgb, var(--edu-text-primary) 12%, transparent);
  transform-origin: center;
}

.edu-loading-card-back {
  opacity: 0.34;
  transform: translate(-50%, -50%) translate(-26px, -12px) rotate(-11deg);
  animation: eduLoadingCardBack 2.6s ease-in-out infinite;
}

.edu-loading-card-mid {
  opacity: 0.58;
  transform: translate(-50%, -50%) translate(24px, -6px) rotate(10deg);
  animation: eduLoadingCardMid 2.6s ease-in-out infinite;
}

.edu-loading-card-front {
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  padding: 18px 18px 16px;
  z-index: 1;
  background:
    linear-gradient(180deg, color-mix(in srgb, var(--edu-bg-surface) 96%, white) 0%, var(--edu-bg-surface) 100%);
  transform: translate(-50%, -50%);
  overflow: hidden;
  animation: eduLoadingCardFront 2.6s ease-in-out infinite;
}

.edu-loading-card-front::after {
  content: '';
  position: absolute;
  inset: 0;
  background:
    linear-gradient(115deg, transparent 22%, color-mix(in srgb, white 54%, transparent) 50%, transparent 78%);
  transform: translateX(-140%);
  animation: eduLoadingShimmer 2.2s ease-in-out infinite;
}

.edu-loading-logo-shell {
  position: relative;
  z-index: 1;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 44px;
  height: 44px;
  border-radius: 14px;
  background: color-mix(in srgb, var(--edu-primary) 12%, var(--edu-bg-surface));
  box-shadow: inset 0 0 0 1px color-mix(in srgb, var(--edu-primary) 18%, transparent);
}

.edu-loading-lines {
  position: relative;
  z-index: 1;
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin-top: 14px;
}

.edu-loading-line {
  display: block;
  height: 10px;
  border-radius: 999px;
  background: color-mix(in srgb, var(--edu-text-tertiary) 14%, transparent);
}

.edu-loading-line-primary {
  width: 88px;
}

.edu-loading-line-secondary {
  width: 116px;
}

.edu-loading-line-tertiary {
  width: 72px;
}

.edu-loading-progress {
  position: relative;
  z-index: 1;
  margin-top: 16px;
  width: 100%;
  height: 8px;
  border-radius: 999px;
  background: color-mix(in srgb, var(--edu-text-tertiary) 12%, transparent);
  overflow: hidden;
}

.edu-loading-progress-bar {
  display: block;
  height: 100%;
  width: 52%;
  border-radius: inherit;
  background: linear-gradient(90deg, var(--edu-primary) 0%, color-mix(in srgb, var(--edu-primary) 58%, white) 100%);
  animation: eduLoadingProgress 1.6s ease-in-out infinite;
}

.edu-loading-logo {
  width: 24px;
  height: 24px;
  object-fit: contain;
}

.edu-page-loading-fade-enter-active,
.edu-page-loading-fade-leave-active {
  transition: opacity 220ms ease;
}

.edu-page-loading-fade-enter-from,
.edu-page-loading-fade-leave-to {
  opacity: 0;
}

@keyframes eduLoadingGlow {
  0% {
    transform: translate(-50%, -50%) scale(0.92);
    opacity: 0.48;
  }
  50% {
    transform: translate(-50%, -50%) scale(1.04);
    opacity: 0.88;
  }
  to {
    transform: translate(-50%, -50%) scale(0.92);
    opacity: 0.48;
  }
}

@keyframes eduLoadingCardBack {
  0%, 100% {
    transform: translate(-50%, -50%) translate(-26px, -12px) rotate(-11deg);
  }

  50% {
    transform: translate(-50%, -50%) translate(-30px, -18px) rotate(-14deg);
  }
}

@keyframes eduLoadingCardMid {
  0%, 100% {
    transform: translate(-50%, -50%) translate(24px, -6px) rotate(10deg);
  }

  50% {
    transform: translate(-50%, -50%) translate(30px, -10px) rotate(13deg);
  }
}

@keyframes eduLoadingCardFront {
  0%, 100% {
    transform: translate(-50%, -50%) translateY(0);
  }

  50% {
    transform: translate(-50%, -50%) translateY(-4px);
  }
}

@keyframes eduLoadingShimmer {
  0% {
    transform: translateX(-140%);
  }

  100% {
    transform: translateX(140%);
  }
}

@keyframes eduLoadingProgress {
  0% {
    transform: translateX(-70%);
  }

  55% {
    transform: translateX(78%);
  }

  100% {
    transform: translateX(168%);
  }
}
</style>
