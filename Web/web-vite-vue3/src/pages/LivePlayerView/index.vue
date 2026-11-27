<template>
  <LivePlayer
    :live-id="liveId"
    @leave="handleLeave"
    @live-room-click="handleLiveRoomClick"
    @logout="handleLogout"
  />
</template>

<script setup lang="ts">
import { useRouter } from 'vue-router';
import { LivePlayer } from '@tencentcloud/livekit-web-vue3';
import { RoutePath } from '../../router';
import { logout } from '../../utils/login';
import type { LiveInfo } from 'tuikit-atomicx-vue3';

defineProps<{
  liveId: string;
}>();

const router = useRouter();

function handleLeave() {
  router.push(RoutePath.LiveList);
}

function handleLiveRoomClick(liveInfo: LiveInfo) {
  router.replace({ path: RoutePath.LivePlayer, query: { liveId: liveInfo.liveId } });
}

async function handleLogout() {
  await logout();
  await router.push(RoutePath.Login);
}
</script>
