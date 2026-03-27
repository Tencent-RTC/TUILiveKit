<template>
  <div class="business-toolbar">
    <button
      v-for="tool in tools"
      :key="tool.id"
      class="toolbar-btn"
      :class="{ active: tool.active }"
      @click="$emit('tool-click', tool.id)"
    >
      <component :is="tool.icon" class="toolbar-icon" />
      <span class="toolbar-label">{{ tool.label }}</span>
    </button>
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue';
import {
  IconBusinessHandRaised as IconHandRaised,
  IconBusinessChatBubble as IconChatBubble,
  IconBusinessChartBar as IconChartBar,
  IconBusinessShare as IconShare,
  IconBusinessVolumeUp as IconVolumeUp,
  IconBusinessArrowsExpand as IconArrowsExpand,
} from '@tencentcloud/uikit-base-component-vue3';

defineEmits<{
  (e: 'tool-click', toolId: string): void;
}>();

interface ToolItem {
  id: string;
  label: string;
  icon: any;
  active?: boolean;
}

const tools = computed<ToolItem[]>(() => [
  { id: 'raise-hand', label: 'Raise Hand', icon: IconHandRaised },
  { id: 'qa', label: 'Q&A', icon: IconChatBubble },
  { id: 'poll', label: 'Polls', icon: IconChartBar },
  { id: 'share', label: 'Share', icon: IconShare },
  { id: 'volume', label: 'Volume', icon: IconVolumeUp },
  { id: 'fullscreen', label: 'Fullscreen', icon: IconArrowsExpand },
]);
</script>
