<template>
  <LivePlayerView
    :live-id="liveId"
    @leave="handleLeave"
    @logout="handleLogout"
  />
</template>

<script setup lang="ts">
import { useRouter } from 'vue-router';
import { LivePlayerView } from '@tencentcloud/livekit-web-vue3';
import { RoutePath } from '../../router';
import { logout } from '../../utils/login';

defineProps<{
  liveId: string;
}>();

const router = useRouter();

function handleLeave() {
  router.push(RoutePath.LiveList);
}

async function handleLogout() {
  await logout();
  await router.push(RoutePath.Login);
}
</script>
