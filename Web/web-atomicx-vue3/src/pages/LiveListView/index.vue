<template>
  <div
    class="live-list-page"
    data-lk-theme="dark"
  >
    <header class="live-list-header">
      <div class="live-list-brand">
        <span class="live-list-logo" />
        LiveKit
      </div>
      <div class="live-list-account">
        <Button
          class="live-list-start"
          :variant="ButtonVariant.Primary"
          :size="ButtonSize.Small"
          @click="router.push(RoutePath.LivePusher)"
        >
          {{ t('Demo.StartLive') }}
        </Button>
        <Avatar
          :src="loginUserInfo?.avatarUrl"
          :size="24"
        />
        <span class="live-list-user-name">{{ loginUserInfo?.userName || loginUserInfo?.userId }}</span>
        <Button
          :size="ButtonSize.Small"
          @click="handleLogout"
        >
          {{ t('Demo.Logout') }}
        </Button>
      </div>
    </header>
    <LiveList @live-room-click="handleLiveRoomClick" />
  </div>
</template>

<script setup lang="ts">
import { useRouter } from 'vue-router';
import { Avatar, LiveList, useLoginState } from 'tuikit-atomicx-vue3';
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import { Button, ButtonSize, ButtonVariant } from '@tencentcloud/livekit-web-vue3';
import { RoutePath } from '../../router';
import { logout } from '../../utils/login';
import './LiveListView.scss';

const router = useRouter();
const { t } = useUIKit();
const { loginUserInfo } = useLoginState();

function handleLiveRoomClick(liveInfo: { liveId: string }) {
  router.push({ path: RoutePath.LivePlayer, query: { liveId: liveInfo.liveId } });
}

async function handleLogout() {
  await logout();
  await router.push(RoutePath.Login);
}
</script>
