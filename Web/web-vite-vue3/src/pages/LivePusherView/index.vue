<template>
  <LivePusher
    :logo-url="logoUrl"
    brand-name="LiveKit"
    :get-watch-link="getWatchLink"
    @leave="handleLeave"
    @logout="handleLogout"
  />
</template>

<script setup lang="ts">
import { watch } from 'vue';
import { useRoute, useRouter } from 'vue-router';
import { useLiveListState } from 'tuikit-atomicx-vue3';
import { LivePusher } from '@tencentcloud/livekit-web-vue3';
import { RoutePath } from '../../router/routePath';
import { logout } from '../../utils/login';

const logoUrl = new URL('../../assets/logo.svg', import.meta.url).href;

const router = useRouter();
const route = useRoute();
const { currentLive } = useLiveListState();

watch(() => currentLive.value?.liveId, (liveId) => {
  if (route.path === RoutePath.LivePusher && (route.query.liveId || '') !== (liveId || '')) {
    router.replace({ query: liveId ? { liveId } : {} });
  }
}, { immediate: true });

function getWatchLink(liveId: string) {
  return new URL(router.resolve({ path: RoutePath.LivePlayer, query: { liveId } }).href, window.location.href).href;
}

function handleLeave() {
  router.push(RoutePath.LiveList);
}

async function handleLogout() {
  await logout();
  await router.push(RoutePath.Login);
}
</script>
