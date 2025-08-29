<template>
  <header class="live-header">
    <div class="header-left" @click="handleHomeClick">
      <img class="header-left-logo" src="../assets/imgs/logo.svg" alt="logo" />
      <div class="header-left-title">LiveKit</div>
    </div>
    <div class="header-right">
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
import { onMounted, ref, defineProps } from 'vue';
import { useRouter, useRoute } from 'vue-router';
import { TUIButton, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { useLoginState, Avatar } from 'tuikit-atomicx-vue3';

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
const loginLoading = ref(false);

async function handleLogin() {
  try {
    loginLoading.value = true;
    const storedData = localStorage.getItem('tuiLive-userInfo') || '{}';
    const liveUserInfo = JSON.parse(storedData);
    await login({
      userId: liveUserInfo.userID,
      userSig: liveUserInfo.userSig,
      sdkAppId: liveUserInfo.SDKAppID,
      testEnv: localStorage.getItem('tuikit-live-env') === 'TestEnv',
    });
  } catch (error) {
    console.error(error);
    router.push({ path: '/login', query: route.query });
  } finally {
    loginLoading.value = false;
  }
};

function handleLogout() {
  logout();
  localStorage.removeItem('tuiLive-userInfo');
  router.push({ path: '/login', query: { from: router.currentRoute.value.path, ...route.query } });
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
  if (loginUserInfo.value && loginUserInfo.value.userId) {
    return;
  }
  await handleLogin();
});

</script>

<style lang="scss" scoped>

.live-header {
  display: flex;
  justify-content: space-between;
  align-items: center;

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

    .header-right-name {
      font-size: 14px;
      font-weight: 400;
    }
  }
}

</style>
