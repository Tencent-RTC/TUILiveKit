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
            {{ t('Demo.LoginHero.Title') }}<em>{{ t('Demo.LoginHero.TitleEmphasis') }}</em>
          </h2>
        </div>
        <div>
          <p class="rv">
            <span class="lead">{{ t('Demo.LoginHero.Lead') }}</span>
            {{ t('Demo.LoginHero.Products') }}
          </p>
        </div>
      </div>
      <div class="tags">
        <span class="tag">{{ t('Demo.LoginHero.TagLowLatency') }}</span>
        <span class="tag">{{ t('Demo.LoginHero.TagCrossPlatform') }}</span>
        <span class="tag">{{ t('Demo.LoginHero.TagWeakNetwork') }}</span>
      </div>
      <div class="art"></div>
    </div>

    <div class="auth-form">
      <div class="inner stagger">
        <div class="form-head">
          <h1>
            {{ credentialsPreset
              ? t('Demo.LoginHero.EnterUserIDToStart')
              : t('Demo.LoginHero.FillInfoToStart') }}
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
            {{ t('Demo.LoginHero.ConsoleGuide') }}
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

      <div class="auth-copy">Tencent RTC · {{ t('Demo.LoginHero.DemoOnly') }}</div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue';
import { useRoute, useRouter } from 'vue-router';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { LoginForm, type LoginFormPayload } from '../../components/login';
import { useLoginPrompts } from '../../components/login/loginPrompts';
import { RoutePath } from '../../router/routePath';
import { loginWithUserInfo, readStoredUserInfo } from '../../utils/login';
import { resolveDocLink } from '../../utils/docLinks';
import {
  genTestUserSig,
  getSDKAppID,
  getSDKSecretKey,
  hasStaticCredentials,
  setCredentials,
} from '../../config/basic-info-config';

const logoZh = new URL('../../assets/tcloud_logo_zh.png', import.meta.url).href;
const logoEn = new URL('../../assets/tcloud_logo_en.png', import.meta.url).href;

const route = useRoute();
const router = useRouter();
const { t, language, setTheme } = useUIKit();
const { promptSDKAppIDMissing, reportLoginError } = useLoginPrompts();

const logoSrc = computed(() => (language.value === 'zh-CN' ? logoZh : logoEn));
const consoleLink = computed(() => resolveDocLink('console', language.value));

const credentialsPreset = hasStaticCredentials();
const initialSDKAppID = getSDKAppID();
const initialSecretKey = getSDKSecretKey();

function readLastUserID(): string {
  const stored = readStoredUserInfo();
  return Number(stored?.SDKAppID) === initialSDKAppID ? stored?.userID || '' : '';
}

const initialUserID = readLastUserID();
const submitting = ref(false);

function openConsole() {
  window.open(consoleLink.value, '_blank', 'noopener,noreferrer');
}

async function goToTargetPage() {
  await router.replace(typeof route.query.from === 'string' ? route.query.from : RoutePath.Home);
}

async function attemptLogin(userID: string): Promise<void> {
  if (submitting.value) {
    return;
  }

  let userSig = '';
  try {
    userSig = genTestUserSig(userID);
  } catch {
    promptSDKAppIDMissing({ onConfirm: openConsole });
    return;
  }

  submitting.value = true;
  try {
    await loginWithUserInfo({ SDKAppID: getSDKAppID(), userID, userSig });
  } catch (error) {
    console.error('[login] failed:', error);
    reportLoginError(error, {
      userID,
      onRecover: () => {
        void attemptLogin(userID);
      },
      onInvalidSignature: openConsole,
    });
    return;
  } finally {
    submitting.value = false;
  }

  await goToTargetPage();
}

function handleSubmit(payload: LoginFormPayload) {
  if (!credentialsPreset) {
    setCredentials({ sdkAppId: payload.sdkAppId, secretKey: payload.secretKey });
  }
  void attemptLogin(payload.userID);
}

onMounted(() => {
  setTheme('light');
});

onBeforeUnmount(() => {
  setTheme('dark');
});
</script>

<style scoped lang="scss" src="./LoginViewGithub.scss"></style>
