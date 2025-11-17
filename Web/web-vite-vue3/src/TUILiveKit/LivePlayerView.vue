<template>
  <div class="live-player-view">
    <LivePlayer :liveId="props.liveId" @leaveLive="emit('leaveLive')" />
  </div>
</template>

<script setup lang="ts">
import { defineProps, defineEmits } from 'vue';
import LivePlayer from './component/LivePlayer';

const props = defineProps<{
  liveId: string;
}>();

const emit = defineEmits(['leaveLive']);
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
