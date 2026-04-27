<template>
  <div class="education-header">
    <div class="header-content">
      <div class="header-left">
        <div class="live-badge">
          <span class="live-dot" />
          <span class="live-text">{{ t('LiveStreaming') }}</span>
        </div>
        <div class="header-info">
          <h2 class="header-title">{{ title || t('Untitled Session') }}</h2>
          <p class="header-subtitle">
            <span class="speaker-label">{{ t('Instructor') }}</span>
            <span class="speaker-divider">·</span>
            <span class="duration">{{ formattedDuration }}</span>
            <span class="speaker-divider">·</span>
            <IconUsers class="audience-icon" />
            <span class="audience-count">{{ audienceCount }}</span>
          </p>
        </div>
      </div>
      <div class="header-actions">
        <button class="action-btn" :title="t('Share')" @click="$emit('action-click')">
          <IconShare class="action-icon" />
        </button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue';
import { IconUsers, IconShare, useUIKit } from '@tencentcloud/uikit-base-component-vue3';

const { t } = useUIKit();

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
