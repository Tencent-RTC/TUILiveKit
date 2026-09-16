<template>
  <header class="live-header">
    <div class="header-left" @click="handleHomeClick">
      <img class="header-left-logo" src="../assets/imgs/logo.svg" alt="logo" />
      <div class="header-left-title">LiveKit</div>
    </div>
    <div class="header-right">
      <div v-if="isLiveListPage && !isH5" class="style-preset-dropdown" ref="presetDropdownRef">
        <button class="preset-dropdown-trigger" @click="togglePresetDropdown">
          <IconSwitchTheme :size="16" class="preset-dropdown-icon" />
          <IconChevronDown :size="12" class="preset-dropdown-arrow" :class="{ open: presetDropdownVisible }" />
        </button>
        <Transition name="preset-fade">
          <ul v-if="presetDropdownVisible" class="preset-dropdown-menu">
            <li
              v-for="option in stylePresetOptions"
              :key="option.value || 'default'"
              class="preset-dropdown-item"
              :class="{ active: currentStylePreset === option.value }"
              @click="handleStylePresetChange(option.value)"
            >
              {{ option.label }}
            </li>
          </ul>
        </Transition>
      </div>
      <TUIButton v-if="isLiveListPage && !isH5" class="btn-start-live" type="primary" @click="gotoPusher">{{ t('Start live') }}</TUIButton>
      <Avatar :src="loginUserInfo?.avatarUrl" :size="24" />
      <div class="header-right-name">
        {{ loginUserInfo?.userName || loginUserInfo?.userId }}
      </div>
      <div v-if="props.loginButtonVisible" class="header-right-tools">
        <TUIButton v-if="!loginUserInfo" :loading="loginLoading" @click="handleLogin">{{ loginLoading ? t('LoginLoading') : t('Login') }}</TUIButton>
        <TUIButton v-else @click="handleLogout">{{ t('Logout') }}</TUIButton>
      </div>
    </div>
  </header>
</template>


<script lang="ts" setup>
import { computed, inject, onMounted, onUnmounted, ref, watch } from 'vue';
import { useRouter, useRoute } from 'vue-router';
import {
  TUIButton,
  useUIKit,
  IconSwitchTheme,
  IconChevronDown,
} from '@tencentcloud/uikit-base-component-vue3';
import { useLoginState, useLiveListState, Avatar } from 'tuikit-atomicx-vue3';
import { isH5 } from '../TUILiveKit/utils/environment';
import { useLoginPrompts } from './login/loginPrompts';

type StylePreset = '' | 'business' | 'education';
type StylePresetController = {
  stylePreset: { value: StylePreset };
  setStylePreset: (nextPreset: StylePreset) => void;
};
const STYLE_PRESET_CONTROLLER_KEY = 'app-style-preset-controller';

const props = defineProps({
  loginButtonVisible: {
    type: Boolean,
    default: true,
  },
});
const router = useRouter();
const route = useRoute();
const { t } = useUIKit();
const { login, loginUserInfo, logout } = useLoginState();
const { currentLive, endLive } = useLiveListState();
const {
  promptLogoutWhileLiving,
  promptEndLiveFailed,
  reportLoginError,
} = useLoginPrompts();
const loginLoading = ref(false);
const isLiveListPage = ref(route.path === '/live-list');
const stylePresetController = inject<StylePresetController | null>(STYLE_PRESET_CONTROLLER_KEY, null);
const stylePresetOptions = computed<Array<{ label: string; value: StylePreset }>>(() => [
  { label: t('Default'), value: '' },
  { label: t('Business'), value: 'business' },
  { label: t('Education'), value: 'education' },
]);
const currentStylePreset = computed(() => stylePresetController?.stylePreset.value || '');
const presetDropdownVisible = ref(false);
const presetDropdownRef = ref<HTMLDivElement>();

function togglePresetDropdown() {
  presetDropdownVisible.value = !presetDropdownVisible.value;
}

function handlePresetDropdownOutside(event: MouseEvent) {
  if (presetDropdownRef.value && !presetDropdownRef.value.contains(event.target as Node)) {
    presetDropdownVisible.value = false;
  }
}

function gotoPusher() {
  router.push({ path: '/live-pusher' });
};

function handleStylePresetChange(nextPreset: StylePreset) {
  stylePresetController?.setStylePreset(nextPreset);
  presetDropdownVisible.value = false;
  if (route.query.stylePreset) {
    const query = { ...route.query };
    delete query.stylePreset;
    router.replace({ path: route.path, query });
  }
}

async function handleLogin() {
  try {
    loginLoading.value = true;
    const storedData = sessionStorage.getItem('tuiLive-userInfo') || '{}';
    const liveUserInfo = JSON.parse(storedData);
    await login({
      userId: liveUserInfo.userID,
      userSig: liveUserInfo.userSig,
      sdkAppId: liveUserInfo.SDKAppID,
      testEnv: localStorage.getItem('tuikit-live-env') === 'TestEnv',
    });
  } catch (error) {
    console.error(error);
    reportLoginError(error, {
      userID: loginUserInfo.value?.userId || '',
      onRecover: () => goToLoginPage(),
      onInvalidSignature: () => goToLoginPage(),
    });
    goToLoginPage();
  } finally {
    loginLoading.value = false;
  }
};

function goToLoginPage() {
  router.push({ path: '/login', query: { from: router.currentRoute.value.path, ...route.query } });
}

function proceedLogout() {
  logout();
  sessionStorage.removeItem('tuiLive-userInfo');
  goToLoginPage();
};

function handleLogout() {
  if (currentLive.value?.liveId) {
    promptLogoutWhileLiving({
      onConfirm: async () => {
        try {
          await endLive();
        } catch (error) {
          console.warn('End live failed when log out:', error);
          promptEndLiveFailed({ onConfirm: () => handleLogout() });
          return;
        }
        proceedLogout();
      },
    });
  } else {
    proceedLogout();
  }
};

function handleHomeClick() {
  const hasVConsole = route.query.vConsole === 'true';
  let currentQuery;
  if (hasVConsole) {
    currentQuery = { vConsole: true };
  }
  router.push({ path: '/live-list', query: currentQuery || {} });
}

onMounted(async () => {
  document.addEventListener('mousedown', handlePresetDropdownOutside);
  if (loginUserInfo.value && loginUserInfo.value.userId) {
    return;
  }
  await handleLogin();
});

onUnmounted(() => {
  document.removeEventListener('mousedown', handlePresetDropdownOutside);
});

watch(
  () => route.path,
  (newPath) => {
    isLiveListPage.value = newPath === '/live-list';
  },
  { immediate: true },
);

</script>

<style lang="scss" scoped>

.live-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  user-select: none;

  .header-left {
    display: flex;
    align-items: center;
    gap: 4px;

    &:hover {
      cursor: pointer;
    }

    .header-left-logo {
      width: 26px;
      height: 24px;
    }

    .header-left-title {
      font-size: 18px;
      font-weight: 600;
      color: var(--text-color-primary);
    }
  }

  .header-right {
    display: flex;
    align-items: center;
    gap: 8px;

    .style-preset-dropdown {
      position: relative;
      display: inline-flex;
      align-items: center;

      .preset-dropdown-trigger {
        display: inline-flex;
        align-items: center;
        gap: 2px;
        height: 32px;
        padding: 0 6px;
        border: none;
        border-radius: 8px;
        background: transparent;
        color: var(--text-color-primary);
        cursor: pointer;
        transition: background 160ms ease;

        &:hover {
          background: color-mix(in srgb, var(--text-color-primary) 10%, transparent);
        }

        .preset-dropdown-icon {
          flex-shrink: 0;
        }

        .preset-dropdown-arrow {
          flex-shrink: 0;
          transition: transform 160ms ease;

          &.open {
            transform: rotate(180deg);
          }
        }
      }

      .preset-dropdown-menu {
        position: absolute;
        top: calc(100% + 6px);
        right: 0;
        min-width: 120px;
        margin: 0;
        padding: 4px;
        list-style: none;
        border-radius: 8px;
        background: var(--bg-color-operate);
        border: 1px solid var(--stroke-color-module);
        box-shadow: 0 6px 20px rgba(0, 0, 0, 0.18);
        z-index: 100;

        .preset-dropdown-item {
          padding: 8px 12px;
          border-radius: 6px;
          font-size: 13px;
          color: var(--text-color-primary);
          cursor: pointer;
          white-space: nowrap;
          transition: background 120ms ease;

          &:hover {
            background: color-mix(in srgb, var(--text-color-primary) 10%, transparent);
          }

          &.active {
            color: var(--button-color-primary-text);
            background: var(--button-color-primary-default);
          }
        }
      }
    }

    .preset-fade-enter-active,
    .preset-fade-leave-active {
      transition: opacity 140ms ease, transform 140ms ease;
    }

    .preset-fade-enter-from,
    .preset-fade-leave-to {
      opacity: 0;
      transform: translateY(-4px);
    }

    .btn-start-live {
      margin-right: 20px;
    }

    .header-right-name {
      max-width: 200px;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
      font-size: 14px;
      font-weight: 400;

      @media screen and (max-width: 480px) {
        max-width: 120px;
      }
    }
  }
}

</style>
