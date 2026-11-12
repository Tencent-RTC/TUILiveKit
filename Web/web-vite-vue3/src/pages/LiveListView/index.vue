<template>
  <LiveList
    :logo-url="logoUrl"
    brand-name="LiveKit"
    @start-live="handleStartLive"
    @live-room-click="handleLiveRoomClick"
    @logout="handleLogout"
  />
</template>

<script setup lang="ts">
import { useRouter } from 'vue-router';
import { LiveList } from '@tencentcloud/livekit-web-vue3';
import { RoutePath } from '../../router/routePath';
import { logout } from '../../utils/login';
import type { LiveInfo } from 'tuikit-atomicx-vue3';

const logoUrl = new URL('../../assets/logo.svg', import.meta.url).href;

const router = useRouter();

function handleStartLive() {
  router.push(RoutePath.LivePusher);
}

function handleLiveRoomClick(liveInfo: LiveInfo) {
  router.push({ path: RoutePath.LivePlayer, query: { liveId: liveInfo.liveId } });
}

async function handleLogout() {
  await logout();
  await router.push(RoutePath.Login);
}
</script>
