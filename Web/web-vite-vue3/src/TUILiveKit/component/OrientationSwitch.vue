<template>
  <div
    class="custom-icon-container"
    :class="{ disabled: currentLive?.liveId }"
    @click="handleOrientationSwitch"
  >
    <IconHorizontalMode
      v-if="currentOrientation === LiveOrientation.Landscape"
      class="custom-icon"
    />
    <IconPortrait
      v-else
      class="custom-icon"
    />
    <span class="custom-text co-guest-text">{{
      currentOrientation === LiveOrientation.Portrait ? t('Portrait') : t('Landscape')
    }}</span>
  </div>
</template>

<script setup lang="ts">
import { ref, watch } from 'vue';
import { TUISeatLayoutTemplate } from '../types/LivePusher';
import {
  useUIKit,
  TUIToast,
  TOAST_TYPE,
  IconPortrait,
  IconHorizontalMode,
} from '@tencentcloud/uikit-base-component-vue3';
import { useLiveListState, LiveOrientation } from 'tuikit-atomicx-vue3';

const { t } = useUIKit();

const { currentLive, updateLiveInfo } = useLiveListState();
const currentOrientation = ref(LiveOrientation.Landscape);
// Store layout template before live starts to restore after live ends
let layoutBeforeLive = TUISeatLayoutTemplate.LandscapeDynamic_1v3;

// Determine if layout template is valid
const isValidLayout = (layout: number | undefined) => layout && layout !== 0;

// Determine if layout is landscape
const isLandscapeLayout = (layout: number) => layout < TUISeatLayoutTemplate.PortraitDynamic_Grid9 && layout >= TUISeatLayoutTemplate.LandscapeDynamic_1v3;

// Determine if layout is portrait
const isPortraitLayout = (layout: number) => layout >= TUISeatLayoutTemplate.PortraitDynamic_Grid9;

watch(
  () => currentLive.value?.layoutTemplate,
  (newVal) => {
    const hasLiveId = !!currentLive.value?.liveId;

    // Handle invalid layout after ending live: restore previous layout
    if (!isValidLayout(newVal) && !hasLiveId) {
      updateLiveInfo({ layoutTemplate: layoutBeforeLive });
      return;
    }

    // Save current layout when not in live session
    if (!hasLiveId && isValidLayout(newVal)) {
      layoutBeforeLive = newVal!;
    }

    // Update orientation based on layout
    if (newVal) {
      if (isLandscapeLayout(newVal)) {
        currentOrientation.value = LiveOrientation.Landscape;
      } else if (isPortraitLayout(newVal)) {
        currentOrientation.value = LiveOrientation.Portrait;
      }
    }
  },
  { immediate: true },
);

// Ensure default landscape mode on initial load
watch(
  () => currentLive.value?.liveId,
  (liveId) => {
    if (!liveId && !isValidLayout(currentLive.value?.layoutTemplate)) {
      updateLiveInfo({ layoutTemplate: layoutBeforeLive });
    }
  },
  { immediate: true },
);

const handleOrientationSwitch = () => {
  if (currentLive.value?.liveId) {
    TUIToast({
      message: t('Cannot switch orientation during live streaming'),
      type: TOAST_TYPE.ERROR,
    });
    return;
  }
  if (currentOrientation.value === LiveOrientation.Portrait) {
    updateLiveInfo({ layoutTemplate: TUISeatLayoutTemplate.LandscapeDynamic_1v3 });
  } else {
    updateLiveInfo({ layoutTemplate: TUISeatLayoutTemplate.PortraitDynamic_Grid9 });
  }
};
</script>

<style scoped lang="scss">
@import '../style/index.scss';

.custom-icon-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 4px;
  min-width: 56px;
  width: auto;
  height: 56px;
  cursor: pointer;
  color: $text-color1;
  border-radius: 12px;
  position: relative;

  .custom-icon {
    @include icon-size-24;
    background: transparent;
  }
  .custom-text {
    @include text-size-12;
  }

  &:not(.disabled):hover {
    box-shadow: 0 0 10px 0 var(--bg-color-mask);
    .custom-icon {
      color: $icon-hover-color;
    }
    .custom-text {
      color: $icon-hover-color;
    }
  }

  &.disabled {
    cursor: not-allowed;
    opacity: 0.5;
    color: $text-color3;
    .custom-icon {
      color: $text-color3;
      cursor: not-allowed;
    }
    .custom-text {
      color: $text-color3;
    }
  }
}

.horizontal-icon {
  mask-image: url('../icons/horizontal.svg');
}

.portrait-icon {
  mask-image: url('../icons/portrait.svg');
}
</style>
