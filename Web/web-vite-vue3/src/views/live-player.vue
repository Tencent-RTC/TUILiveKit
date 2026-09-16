<template>
  <div class="live-player-container">
    <LivePlayerView v-if="loginUserInfo" :live-id="liveId as string" @leave-live="leaveLive" />
  </div>
</template>

<script lang="ts" setup>
import { onMounted } from 'vue';
import { useRouter, useRoute } from 'vue-router';
import { LivePlayerView } from '../TUILiveKit';
import { useLoginState } from 'tuikit-atomicx-vue3';

const { loginUserInfo } = useLoginState();
const router = useRouter();
const route = useRoute();
const { liveId } = route.query;

function leaveLive() {
  router.push({ path: '/live-list' });
}

// Login guard — previously provided by the removed LiveHeader component.
// Without it an unauthenticated viewer would hit a blank player page.
onMounted(() => {
  if (!loginUserInfo.value?.userId) {
    router.replace({ path: '/login', query: { from: route.path, ...route.query } });
  }
});
</script>

<style lang="scss" scoped>
.live-player-container {
  position: relative;
  width: 100%;
  height: 100vh;
  overflow: hidden;
  background-color: #000;
  color: #fff;
}
</style>
