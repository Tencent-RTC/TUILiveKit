<template>
  <div class="auth-wrap">
    <div class="auth-top">
      <div class="brand">
        <img class="brand-logo" :src="logoSrc" alt="Tencent Cloud" />
      </div>
    </div>

    <div class="auth-hero">
      <div class="ht reveal">
        <div><span class="rv eyebrow">Tencent RTC</span></div>
        <div>
          <h2 class="rv">
            {{ t('Real-time interaction') }}<em>{{ t('in milliseconds') }}</em>
          </h2>
        </div>
        <div>
          <p class="rv">
            <span class="lead">{{ t('Build it into your app with a hundred lines of code in thirty minutes') }}</span>
            {{ t('Voice Call · Video Call · Interactive Live · Real-time Messaging') }}
          </p>
        </div>
      </div>
      <div class="tags">
        <span class="tag">{{ t('Ultra-low latency') }}</span>
        <span class="tag">{{ t('Cross-platform') }}</span>
        <span class="tag">{{ t('Weak network optimization') }}</span>
      </div>
      <div class="art"></div>
    </div>

    <div class="auth-form">
      <div class="inner stagger">
        <div class="form-head">
          <h1>
            {{ credentialsPreset
              ? t('Enter your userID to get started')
              : t('Fill in the information below to get started') }}
          </h1>
        </div>

        <a
          v-if="!credentialsPreset"
          class="guide-callout"
          :href="consoleLink"
          target="_blank"
          rel="noopener noreferrer"
        >
          <span class="callout-icon">
            <svg width="16" height="16" viewBox="0 0 24 24" fill="none">
              <circle cx="12" cy="12" r="10" stroke="currentColor" stroke-width="2" />
              <path d="M12 16v-4" stroke="currentColor" stroke-width="2" stroke-linecap="round" />
              <path d="M12 8h.01" stroke="currentColor" stroke-width="2" stroke-linecap="round" />
            </svg>
          </span>
          <span class="callout-text">
            {{ t('Get your SDKAppID and secret key from the console') }}
          </span>
          <span class="callout-arrow">
            <svg width="14" height="14" viewBox="0 0 24 24" fill="none">
              <path d="m9 6 6 6-6 6" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
            </svg>
          </span>
        </a>

        <div class="form-body">
          <LoginForm
            :sdk-app-id="initialSDKAppID"
            :secret-key="initialSecretKey"
            :user-id="initialUserID"
            :credentials-preset="credentialsPreset"
            :loading="submitting"
            @submit="handleSubmit"
          />
        </div>
      </div>

      <div class="auth-copy">Tencent RTC · {{ t('For feature demonstration only') }}</div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { computed, onMounted, onBeforeUnmount, ref } from 'vue';
import { useRouter, useRoute } from 'vue-router';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { useLoginState } from 'tuikit-atomicx-vue3';
import { LoginForm } from '../components/login';
import { useLoginPrompts } from '../components/login/loginPrompts';
import logoZh from '../assets/imgs/tcloud_logo_zh.png';
import logoEn from '../assets/imgs/tcloud_logo_en.png';
import { deepClone } from '../utils/utils';
import { resolveDocLink } from '../utils/docLinks';
import {
  genTestUserSig,
  getSDKAppID,
  getSDKSecretKey,
  hasStaticCredentials,
  setCredentials,
} from '../config/basic-info-config';

const router = useRouter();
const route = useRoute();
const { t, language, setTheme } = useUIKit();
const { login } = useLoginState();
const { promptSDKAppIDMissing, reportLoginError } = useLoginPrompts();

const logoSrc = computed(() => (language.value === 'zh-CN' ? logoZh : logoEn));

const consoleLink = computed(() => resolveDocLink('console', language.value));

// When SDKAppID and the secret key are already filled in
// src/config/basic-info-config.js, the login page only asks for the userID.
const credentialsPreset = hasStaticCredentials();

// Prefill from the credentials already entered in this session (or from the
// fallback values still hard-coded in src/config/basic-info-config.js).
const initialSDKAppID = getSDKAppID();
const initialSecretKey = getSDKSecretKey();

function readLastUserID(): string {
  try {
    const raw = sessionStorage.getItem('tuiLive-userInfo');
    if (!raw) {
      return '';
    }
    const parsed = JSON.parse(raw);
    return Number(parsed?.SDKAppID) === initialSDKAppID ? parsed?.userID || '' : '';
  } catch (error) {
    return '';
  }
}

const initialUserID = readLastUserID();
const submitting = ref(false);

const openConsole = () => {
  window.open(consoleLink.value, '_blank', 'noopener,noreferrer');
};

const goToTargetPage = () => {
  const currentQuery = deepClone(route.query);
  delete currentQuery.from;
  router.push({ path: route.query.from as string || '/live-list', query: currentQuery });
};

const handleLoginError = (error: unknown, userID: string) => {
  console.error('[login] failed:', error);
  reportLoginError(error, {
    userID,
    onRecover: () => {
      void attemptLogin(userID);
    },
    onInvalidSignature: openConsole,
  });
};

// Logging in here rather than leaving it to the router guard is what makes the
// signature and network failures reportable on this page.
const attemptLogin = async (userID: string): Promise<void> => {
  if (submitting.value) {
    return;
  }

  let userSig = '';
  try {
    userSig = genTestUserSig(userID);
  } catch (error) {
    promptSDKAppIDMissing({ onConfirm: openConsole });
    return;
  }

  submitting.value = true;
  try {
    await login({
      userId: userID,
      userSig,
      sdkAppId: getSDKAppID(),
      testEnv: localStorage.getItem('tuikit-live-env') === 'TestEnv',
    });
  } catch (error) {
    handleLoginError(error, userID);
    return;
  } finally {
    submitting.value = false;
  }

  sessionStorage.setItem('tuiLive-userInfo', JSON.stringify({
    SDKAppID: getSDKAppID(),
    userID,
    userSig,
  }));
  goToTargetPage();
};

const handleSubmit = (payload: {
  sdkAppId: number;
  secretKey: string;
  userID: string;
}) => {
  // Outside preset mode, whatever is typed here takes the place of the values
  // that used to be hard-coded in src/config/basic-info-config.js.
  if (!credentialsPreset) {
    setCredentials({ sdkAppId: payload.sdkAppId, secretKey: payload.secretKey });
  }
  void attemptLogin(payload.userID);
};

// The login page uses the light brand look, while the live pages stay dark.
// Theme is global in the UIKit, so restore it when leaving this route.
onMounted(() => {
  setTheme('light');
});

onBeforeUnmount(() => {
  setTheme('dark');
});
</script>

<style scoped lang="scss">
// Login page visual system, aligned with the LiveKit flow prototype home screen:
// left half is the brand hero on Tencent RTC blue, right half is the light form column.

$acc: #1c66e5;
$acc-lit: #1559cc;
$txt: #1a2029;
$txt-2: #4a5462;
$txt-4: #7d8794;
$e-out: cubic-bezier(0.22, 1, 0.36, 1);
$e-io: cubic-bezier(0.45, 0, 0.25, 1);

@keyframes heroWipe {
  from { clip-path: inset(0 100% 0 0); }
  to { clip-path: inset(0 0 0 0); }
}

@keyframes heroFloat {
  0%, 100% { transform: translateY(0); }
  50% { transform: translateY(-8px); }
}

@keyframes artIn {
  from { opacity: 0; transform: translateY(38px) scale(0.96); filter: blur(6px); }
  to { opacity: 1; transform: none; filter: blur(0); }
}

@keyframes revealUp {
  from { opacity: 0; transform: translateY(112%) skewY(3deg); }
  to { opacity: 1; transform: none; }
}

@keyframes slideL {
  from { opacity: 0; transform: translateX(-42px); }
  to { opacity: 1; transform: none; }
}

@keyframes slideR {
  from { opacity: 0; transform: translateX(42px); }
  to { opacity: 1; transform: none; }
}

@keyframes fadeUp {
  from { opacity: 0; transform: translateY(12px); }
  to { opacity: 1; transform: none; }
}

.auth-wrap {
  position: relative;
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-template-rows: 1fr;
  width: 100%;
  min-height: 100vh;
  background: #f7f9fc;
  color: $txt;
}

.auth-top {
  position: absolute;
  top: 0;
  left: 0;
  z-index: 3;
  display: flex;
  align-items: center;
  height: 60px;
  padding: 0 30px;
}

.brand {
  display: inline-flex;
  align-items: center;
  transition: opacity 0.2s $e-out;

  &:hover {
    opacity: 0.86;
  }
}

.brand-logo {
  display: block;
  width: auto;
  height: 26px;
  filter: drop-shadow(0 1px 4px rgba(4, 18, 52, 0.35));
}

/* ── Left half: brand hero ── */
.auth-hero {
  position: relative;
  display: flex;
  flex-direction: column;
  justify-content: flex-start;
  padding: 132px 8% 70px;
  overflow: hidden;
  background:
    linear-gradient(180deg, rgba(3, 15, 46, 0.62) 0%, rgba(3, 15, 46, 0.26) 14%, rgba(3, 15, 46, 0) 32%),
    linear-gradient(196deg, #16499f 0%, #1c66e5 46%, #3a7cf0 100%);
  animation: heroWipe 0.78s $e-out both;

  &::before {
    position: absolute;
    inset: 0;
    pointer-events: none;
    content: "";
    background:
      radial-gradient(ellipse 86% 52% at 78% 98%, rgba(255, 255, 255, 0.18), transparent 64%),
      radial-gradient(ellipse 64% 34% at 2% -10%, rgba(2, 13, 42, 0.5), transparent 70%);
  }

  &::after {
    position: absolute;
    inset: 0;
    pointer-events: none;
    content: "";
    background-image:
      linear-gradient(to right, rgba(255, 255, 255, 0.07) 1px, transparent 1px),
      linear-gradient(to bottom, rgba(255, 255, 255, 0.07) 1px, transparent 1px);
    background-size: 40px 40px;
    opacity: 0.5;
    mask-image: radial-gradient(ellipse 80% 70% at 30% 30%, #000, transparent 78%);
    -webkit-mask-image: radial-gradient(ellipse 80% 70% at 30% 30%, #000, transparent 78%);
  }

  .ht {
    position: relative;
    z-index: 1;
    max-width: 490px;
  }

  .eyebrow {
    display: inline-flex;
    align-items: center;
    gap: 7px;
    margin-bottom: 20px;
    font-size: 12.5px;
    font-weight: 500;
    color: rgba(255, 255, 255, 0.75);
    text-transform: uppercase;
    letter-spacing: 0.15em;

    &::before {
      flex: 0 0 auto;
      width: 22px;
      height: 1px;
      content: "";
      background: rgba(255, 255, 255, 0.5);
    }
  }

  h2 {
    margin: 0 0 20px;
    font-size: 48px;
    font-weight: 600;
    line-height: 1.16;
    color: #fff;
    letter-spacing: -0.036em;
    text-shadow: 0 2px 14px rgba(8, 34, 90, 0.22);

    em {
      padding-bottom: 0.06em;
      font-style: normal;
      font-weight: 600;
      color: transparent;
      background: linear-gradient(96deg, #c6f0ff, #8fe0ff 60%, #66d4ff);
      -webkit-background-clip: text;
      background-clip: text;
      -webkit-text-fill-color: transparent;
    }
  }

  p {
    max-width: 452px;
    margin: 0;
    font-size: 15.5px;
    line-height: 1.82;
    color: rgba(255, 255, 255, 0.82);
    letter-spacing: -0.003em;

    .lead {
      display: block;
      margin-bottom: 7px;
      font-size: 17.5px;
      font-weight: 500;
      color: #fff;
      letter-spacing: -0.012em;
    }
  }

  .tags {
    position: relative;
    z-index: 1;
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    max-width: 480px;
    margin-top: 30px;
    gap: 0 20px;
  }

  .tag {
    position: relative;
    display: inline-flex;
    align-items: center;
    gap: 7px;
    padding: 5px 0;
    font-size: 14px;
    color: rgba(255, 255, 255, 0.9);
    letter-spacing: -0.004em;
    animation: slideL 0.62s $e-out both;

    &:nth-child(1) { animation-delay: 0.52s; }
    &:nth-child(2) { animation-delay: 0.6s; }
    &:nth-child(3) { animation-delay: 0.68s; }

    &::before {
      flex: 0 0 auto;
      width: 4.5px;
      height: 4.5px;
      content: "";
      background: rgba(198, 240, 255, 0.92);
      border-radius: 50%;
      box-shadow: 0 0 7px rgba(160, 225, 255, 0.75);
    }
  }

  .art {
    position: absolute;
    right: 0;
    bottom: 0;
    z-index: 1;
    width: 78%;
    height: 56%;
    pointer-events: none;
    background: url("https://cloudcache.tencent-cloud.com/qcloud/ui/static/static_source_business/97991446-f2ba-4ebd-925f-f9ccba214a0e.png") no-repeat right bottom;
    background-size: contain;
    filter: drop-shadow(0 12px 30px rgba(6, 26, 70, 0.28));
    animation: artIn 0.95s $e-out 0.36s both, heroFloat 6.5s $e-io 1.3s infinite;
  }
}

.reveal > * {
  overflow: hidden;
}

.reveal .rv {
  display: block;
  animation: revealUp 0.82s $e-out both;
}

.reveal .rv.eyebrow {
  display: inline-flex;
}

.reveal > *:nth-child(1) .rv { animation-delay: 0.06s; }
.reveal > *:nth-child(2) .rv { animation-delay: 0.16s; }
.reveal > *:nth-child(3) .rv { animation-delay: 0.26s; }

/* ── Right half: form column ── */
.auth-form {
  position: relative;
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 60px 8%;
  background:
    radial-gradient(ellipse 84% 58% at 88% 2%, rgba(28, 102, 229, 0.07), transparent 60%),
    radial-gradient(ellipse 70% 50% at 8% 100%, rgba(28, 102, 229, 0.045), transparent 62%),
    linear-gradient(170deg, #ffffff, #fafbfe 48%, #f4f7fc);

  &::before {
    position: absolute;
    inset: 0;
    pointer-events: none;
    content: "";
    background-image:
      linear-gradient(to right, rgba(20, 32, 56, 0.035) 1px, transparent 1px),
      linear-gradient(to bottom, rgba(20, 32, 56, 0.035) 1px, transparent 1px);
    background-size: 36px 36px;
    opacity: 0.6;
    mask-image: radial-gradient(ellipse 74% 66% at 50% 46%, transparent 16%, #000 82%);
    -webkit-mask-image: radial-gradient(ellipse 74% 66% at 50% 46%, transparent 16%, #000 82%);
  }

  .inner {
    position: relative;
    z-index: 1;
    width: 100%;
    max-width: 404px;
    margin: 0 auto;
    animation: slideR 0.7s $e-out 0.18s both;
  }
}

.stagger > * {
  animation: fadeUp 0.46s $e-out both;
}

.stagger > *:nth-child(1) { animation-delay: 0.03s; }
.stagger > *:nth-child(2) { animation-delay: 0.08s; }
.stagger > *:nth-child(3) { animation-delay: 0.13s; }

.form-head {
  margin-bottom: 14px;

  h1 {
    margin: 0;
    font-size: 22px;
    font-weight: 600;
    line-height: 1.38;
    color: $txt;
    letter-spacing: -0.026em;
  }
}

.guide-callout {
  display: flex;
  align-items: flex-start;
  width: 100%;
  padding: 11px 12px;
  margin-bottom: 18px;
  color: $txt-2;
  text-align: left;
  text-decoration: none;
  cursor: pointer;
  background: rgba(28, 102, 229, 0.06);
  border: 1px solid rgba(28, 102, 229, 0.2);
  border-radius: 10px;
  gap: 9px;
  transition:
    background 0.2s $e-out,
    border-color 0.2s $e-out,
    box-shadow 0.2s $e-out;

  &:hover {
    background: rgba(28, 102, 229, 0.1);
    border-color: rgba(28, 102, 229, 0.34);
    box-shadow: 0 4px 14px -6px rgba(28, 102, 229, 0.4);

    .callout-arrow {
      transform: translateX(3px);
    }
  }

  &:focus-visible {
    outline: 2px solid $acc;
    outline-offset: 2px;
  }

  .callout-icon {
    display: flex;
    flex: 0 0 auto;
    margin-top: 1px;
    color: $acc;
  }

  .callout-text {
    flex: 1;
    min-width: 0;
    font-size: 12.5px;
    line-height: 1.6;
    letter-spacing: -0.002em;
  }

  .callout-arrow {
    display: flex;
    flex: 0 0 auto;
    margin-top: 2px;
    color: $acc-lit;
    transition: transform 0.24s $e-out;
  }
}

.auth-copy {
  position: absolute;
  bottom: 18px;
  left: 50%;
  z-index: 2;
  font-size: 10.5px;
  color: $txt-4;
  letter-spacing: -0.002em;
  white-space: nowrap;
  pointer-events: none;
  transform: translateX(-50%);
}

@media (max-width: 820px) {
  .auth-wrap {
    grid-template-columns: 1fr;
  }

  .auth-hero {
    display: none;
  }

  // The hero is gone here, so the white wordmark needs its own deep band.
  .auth-top {
    width: 100%;
    height: 58px;
    padding: 0 22px;
    background: linear-gradient(180deg, #123f8e, #1a5fd6);
  }

  .auth-form {
    padding: 96px 26px 60px;
  }
}

@media (prefers-reduced-motion: reduce) {
  .auth-hero,
  .auth-hero .art,
  .auth-hero .tag,
  .reveal .rv,
  .auth-form .inner,
  .stagger > * {
    animation: none;
  }
}
</style>
