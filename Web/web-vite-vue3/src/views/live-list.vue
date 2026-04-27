<template>
  <div class="live-list-container">
    <LiveHeader class="live-list-header" />
    <LiveListView @live-room-click="handleLiveRoomClick" />
  </div>
</template>

<script lang="ts" setup>
import { useRouter, useRoute } from 'vue-router';
import { LiveInfo, useLoginState } from 'tuikit-atomicx-vue3';
import { useUIKit, TUIMessageBox, useStylePreset } from '@tencentcloud/uikit-base-component-vue3';
import { LiveListView } from '../TUILiveKit';
import LiveHeader from '../components/LiveHeader.vue';
import { isH5 } from '../TUILiveKit/utils/environment';

const router = useRouter();
const route = useRoute();
const { loginUserInfo } = useLoginState();
const { t } =  useUIKit();
const { presetName } = useStylePreset();
type StylePreset = '' | 'business' | 'education';

function getStylePresetFromContext(): StylePreset {
  if (isH5) {
    return '';
  }
  const presetFromQuery = route.query.stylePreset;
  if (presetFromQuery === 'business' || presetFromQuery === 'education') {
    return presetFromQuery;
  }
  if (presetName.value === 'business' || presetName.value === 'education') {
    return presetName.value;
  }
  return '';
}

function handleLiveRoomClick(liveInfo: LiveInfo) {
  if (loginUserInfo.value?.userId === liveInfo.liveOwner?.userId) {
    TUIMessageBox.alert({
      title: t('Warning'),
      content: t('Unable to view own live'),
    });
    return;
  }

  if (liveInfo?.liveId) {
    const query: Record<string, string> = { ...route.query as Record<string, string>, liveId: liveInfo.liveId };
    const stylePreset = getStylePresetFromContext();
    if (stylePreset) {
      query.stylePreset = stylePreset;
    }
    router.push({ path: '/live-player', query });
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
