<template>
  <div class="business-header">
    <div class="header-content">
      <div class="header-left">
        <div class="live-badge">
          <span class="live-dot" />
          <span class="live-text">LIVE</span>
        </div>
        <div class="header-info">
          <h2 class="header-title">{{ title || 'Untitled Session' }}</h2>
          <p class="header-subtitle">
            <span class="speaker-label">Speaker</span>
            <span class="speaker-divider">·</span>
            <span class="duration">{{ formattedDuration }}</span>
            <span class="speaker-divider">·</span>
            <IconUsers class="audience-icon" />
            <span class="audience-count">{{ audienceCount }}</span>
          </p>
        </div>
      </div>
      <div class="header-actions">
        <button class="action-btn" :title="'Share'" @click="$emit('action-click')">
          <IconShare class="action-icon" />
        </button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue';
import {
  IconBusinessUsers as IconUsers,
  IconBusinessShare as IconShare,
} from '@tencentcloud/uikit-base-component-vue3';

const props = defineProps<{
  title?: string;
  audienceCount?: number;
  networkQuality?: 'good' | 'fair' | 'poor';
  startTime?: number;
}>();

defineEmits(['action-click']);

// Duration counter
const elapsed = ref(0);
let timer: ReturnType<typeof setInterval>;

onMounted(() => {
  timer = setInterval(() => {
    if (props.startTime) {
      elapsed.value = Math.floor((Date.now() - props.startTime) / 1000);
    } else {
      elapsed.value += 1;
    }
  }, 1000);
});

onUnmounted(() => clearInterval(timer));

const formattedDuration = computed(() => {
  const h = Math.floor(elapsed.value / 3600);
  const m = Math.floor((elapsed.value % 3600) / 60);
  const s = elapsed.value % 60;
  return `${String(h).padStart(2, '0')}:${String(m).padStart(2, '0')}:${String(s).padStart(2, '0')}`;
});
</script>
