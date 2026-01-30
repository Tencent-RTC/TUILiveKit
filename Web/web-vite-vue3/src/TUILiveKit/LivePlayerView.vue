<template>
  <div class="live-player-view">
    <LivePlayer :liveId="props.liveId" @leaveLive="emit('leaveLive')" />
  </div>
</template>

<script setup lang="ts">
import { onMounted } from 'vue';
import { TUIMessageBox, useUIKit } from '@tencentcloud/uikit-base-component-vue3';
import LivePlayer from './component/LivePlayer';
import { isSafariBrowser } from './utils/utils';

const props = defineProps<{
  liveId: string;
}>();

const emit = defineEmits(['leaveLive']);
const { t } = useUIKit();

onMounted(() => {
  let timer: number;
  timer = setTimeout(() => {
    clearTimeout(timer);
    const [navigation] = performance.getEntriesByType('navigation');
    if (navigation?.type === 'reload' && isSafariBrowser() && document.querySelector('.tcplayer')) {
      TUIMessageBox.alert({
        content: t('Content is ready. Click the button to start playback'),
        confirmText: t('Play'),
        showClose: false,
      });
    }
  }, 3000) as unknown as number;
})
</script>

<style lang="scss" scoped>
.live-player-view {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  align-items: center;
}
</style>
