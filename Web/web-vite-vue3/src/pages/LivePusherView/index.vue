<template>
  <LivePusher
    :logo-url="logoUrl"
    brand-name="LiveKit"
    @leave="handleLeave"
    @logout="handleLogout"
  />
</template>

<script setup lang="ts">
import { useRouter } from 'vue-router';
import { LivePusher } from '@tencentcloud/livekit-web-vue3';
import { RoutePath } from '../../router/routePath';
import { logout } from '../../utils/login';

const logoUrl = new URL('../../assets/logo.svg', import.meta.url).href;

const router = useRouter();

function handleLeave() {
  router.push(RoutePath.LiveList);
}

async function handleLogout() {
  await logout();
  await router.push(RoutePath.Login);
}
</script>
