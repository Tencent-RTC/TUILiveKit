<template>
  <div class="live-list-container">
    <LiveHeader class="live-list-header" />
    <LiveListView @live-room-click="handleLiveRoomClick" />
  </div>
</template>

<script lang="ts" setup>
import { useRouter, useRoute } from 'vue-router';
import { LiveInfo, useLoginState } from 'tuikit-atomicx-vue3';
import { useUIKit, TUIMessageBox } from '@tencentcloud/uikit-base-component-vue3';
import { LiveListView } from '@tencentcloud/livekit-web-vue3';
import LiveHeader from '../components/LiveHeader.vue';

const router = useRouter();
const route = useRoute();
const { loginUserInfo } = useLoginState();
const { t } =  useUIKit();

function handleLiveRoomClick(liveInfo: LiveInfo) {
  if (loginUserInfo.value?.userId === liveInfo.liveOwner?.userId) {
    TUIMessageBox.alert({
      title: t('Warning'),
      content: t('Unable to view own live'),
    });
    return;
  }

  if (liveInfo?.liveId) {
    router.push({ path: '/live-player', query: { ...route.query, liveId: liveInfo.liveId } });
  }
}

</script>

<style lang="scss" scoped>
.live-list-container {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
  background-color: var(--bg-color-topbar);
  color: var(--text-color-primary);
  overflow: auto;
  box-sizing: border-box;

  .live-list-header {
    padding: 16px;
  }
}
</style>
