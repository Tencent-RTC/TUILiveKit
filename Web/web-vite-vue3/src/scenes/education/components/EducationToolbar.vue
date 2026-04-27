<template>
  <div class="education-toolbar" role="toolbar">
    <div class="toolbar-group toolbar-left">
      <button class="icon-btn" :title="isMuted ? t('Turn on sound') : t('Mute sound')" @click="$emit('toggle-mute')">
        <svg v-if="isMuted" viewBox="0 0 24 24" fill="none">
          <path d="M4 10h4l5-4v12l-5-4H4z" />
          <path d="M17 9l4 4" />
          <path d="M21 9l-4 4" />
        </svg>
        <svg v-else viewBox="0 0 24 24" fill="none">
          <path d="M4 10h4l5-4v12l-5-4H4z" />
          <path d="M17 9a4 4 0 0 1 0 6" />
          <path d="M19.5 7a7 7 0 0 1 0 10" />
        </svg>
      </button>

      <input
        class="volume-slider"
        type="range"
        min="0"
        max="100"
        :value="Math.round(volume)"
        @input="handleVolumeInput"
      >

      <span class="viewer-count">
        <svg class="viewer-icon" viewBox="0 0 24 24" fill="none">
          <path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" />
          <circle cx="9" cy="7" r="4" />
        </svg>
        {{ viewerCount }}
      </span>
    </div>

    <div class="toolbar-group toolbar-right">
      <button class="icon-btn" :title="t('Setting')" @click="$emit('open-settings')">
        <svg viewBox="0 0 24 24" fill="none">
          <circle cx="12" cy="12" r="3" />
          <path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1-2.83 2.83l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-4 0v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83-2.83l.06-.06A1.65 1.65 0 0 0 4.68 15a1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1 0-4h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 2.83-2.83l.06.06A1.65 1.65 0 0 0 9 4.68a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 4 0v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 2.83l-.06.06A1.65 1.65 0 0 0 19.4 9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 0 4h-.09a1.65 1.65 0 0 0-1.51 1z" />
        </svg>
      </button>

      <button class="icon-btn" :title="isFullscreen ? t('Exit full screen') : t('Enter full screen')" @click="$emit('toggle-fullscreen')">
        <svg v-if="!isFullscreen" viewBox="0 0 24 24" fill="none">
          <path d="M8 4H4v4" />
          <path d="M16 4h4v4" />
          <path d="M8 20H4v-4" />
          <path d="M16 20h4v-4" />
        </svg>
        <svg v-else viewBox="0 0 24 24" fill="none">
          <path d="M4 8h4V4" />
          <path d="M16 4v4h4" />
          <path d="M4 16h4v4" />
          <path d="M20 16h-4v4" />
        </svg>
      </button>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useUIKit } from '@tencentcloud/uikit-base-component-vue3';

const { t } = useUIKit();

defineProps<{
  isMuted: boolean;
  isFullscreen: boolean;
  volume: number;
  viewerCount: number;
}>();

const emit = defineEmits([
  'toggle-mute',
  'change-volume',
  'toggle-fullscreen',
  'open-settings',
]);

function handleVolumeInput(event: Event) {
  const target = event.target as HTMLInputElement;
  emit('change-volume', Number(target.value));
}
</script>

<style scoped lang="scss">
.education-toolbar {
  position: absolute;
  left: 0;
  right: 0;
  bottom: 0;
  z-index: 10;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 16px;
  padding: 8px 16px;
  background: var(--edu-control-bg);
  backdrop-filter: saturate(180%) blur(20px);
  border-top: 1px solid var(--edu-control-border);
  box-shadow: 0 -1px 4px rgba(0, 0, 0, 0.04);
}

.toolbar-group {
  display: flex;
  align-items: center;
  gap: 10px;
  min-width: 0;
}

.toolbar-left {
  flex: 1;
}

.volume-slider {
  width: 100px;
  height: 4px;
  margin: 0 2px;
  border-radius: 999px;
  appearance: none;
  border: 0;
  background: var(--edu-control-slider-track);
  accent-color: var(--edu-control-slider-filled);

  &::-webkit-slider-thumb {
    appearance: none;
    width: 12px;
    height: 12px;
    border-radius: 50%;
    border: 0;
    background: var(--edu-control-slider-thumb);
    box-shadow: 0 0 4px rgba(0, 0, 0, 0.15);
    cursor: pointer;
  }

  &::-moz-range-thumb {
    width: 12px;
    height: 12px;
    border-radius: 50%;
    border: 0;
    background: var(--edu-control-slider-thumb);
    cursor: pointer;
  }
}

.viewer-count {
  display: inline-flex;
  align-items: center;
  gap: 4px;
  margin-left: 4px;
  font-size: 12px;
  color: var(--edu-control-text);
  font-variant-numeric: tabular-nums;
}

.viewer-icon {
  width: 14px;
  height: 14px;
  stroke: currentColor;
  stroke-width: 1.8;
  stroke-linecap: round;
  stroke-linejoin: round;
  flex-shrink: 0;
}

.icon-btn {
  width: 32px;
  height: 32px;
  border: 0;
  background: transparent;
  color: var(--edu-control-icon);
  border-radius: 8px;
  cursor: pointer;
  display: inline-flex;
  align-items: center;
  justify-content: center;

  svg {
    width: 18px;
    height: 18px;
    stroke: currentColor;
    stroke-width: 1.8;
    stroke-linecap: round;
    stroke-linejoin: round;
  }
}
</style>
