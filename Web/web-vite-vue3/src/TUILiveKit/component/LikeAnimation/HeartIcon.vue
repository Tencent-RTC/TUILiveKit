<template>
  <svg
    :width="size"
    :height="size"
    viewBox="0 0 88 88"
    xmlns="http://www.w3.org/2000/svg"
    :style="shadowStyle"
  >
    <!-- Heart with semi-transparent fill and white outline (matching iOS) -->
    <!-- Fill: colored with transparency -->
    <path
      :d="heartPath"
      :fill="color"
      fill-opacity="0.5"
    />
    <!-- Outline: white border -->
    <path
      :d="heartPath"
      fill="none"
      stroke="white"
      stroke-width="2.5"
      stroke-linecap="round"
      stroke-linejoin="round"
    />
  </svg>
</template>

<script setup lang="ts">
import { computed } from 'vue';

interface Props {
  size?: number | string;
  color?: string;
  showShadow?: boolean;
}

const props = withDefaults(defineProps<Props>(), {
  size: 36,
  color: '#FF3B30',
  showShadow: true,
});

// Shadow style (subtle white glow)
const shadowStyle = computed(() => {
  if (!props.showShadow) return {};
  return {
    filter: `drop-shadow(0 0 2px rgba(255, 255, 255, 0.6))`,
  };
});

// Wider heart path - more square-like shape (matching iOS)
// Made the heart wider by extending the curves horizontally
const heartPath = `
  M 44 72
  C 18 50, 8 38, 8 28
  C 8 16, 18 8, 30 8
  C 38 8, 43 13, 44 16
  C 45 13, 50 8, 58 8
  C 70 8, 80 16, 80 28
  C 80 38, 70 50, 44 72
  Z
`;
</script>
