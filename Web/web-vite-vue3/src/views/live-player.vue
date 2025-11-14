<template>
  <div class="live-player-container">
    <LiveHeader v-show="!isMobile" class="live-player-header" :login-button-visible="false"></LiveHeader>
    <LivePlayerView v-if="loginUserInfo" :live-id="liveId as string" @leave-live="leaveLive" />
  </div>
</template>

<script lang="ts" setup>
import { useRouter, useRoute } from 'vue-router';
import { useLoginState } from 'tuikit-atomicx-vue3';
import { LivePlayerView, isMobile } from '../TUILiveKit';
import LiveHeader from '../components/LiveHeader.vue';
import { deepClone } from '../utils/utils';

const { loginUserInfo } = useLoginState();

const router = useRouter();
const route = useRoute();
const { liveId } = route.query;

function leaveLive() {
  const currentQuery = deepClone(route.query);
  delete currentQuery.liveId;
  router.push({ path: '/live-list', query: currentQuery });
}

</script>

<style lang="scss" scoped>
.live-player-container {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
  background-color: var(--bg-color-topbar);
  color: var(--text-color-primary);
  overflow: auto;
  box-sizing: border-box;

  .live-player-header {
    padding: 16px;
  }
}
</style>
